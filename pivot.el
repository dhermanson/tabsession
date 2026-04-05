;;; pivot.el --- pivot between activities with tab-bar groups -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "31.0"))

(require 'cl-lib)
(require 'subr-x)
(require 'tab-bar)
(require 'seq)

;;; Customization

(defgroup pivot nil
  "Session management using tab-bar groups."
  :group 'convenience)

(defface pivot-quick-select-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for keys in the quick session selector."
  :group 'pivot)

(defface pivot-quick-select-session
  '((t :inherit default))
  "Face used for session names in the quick session selector."
  :group 'pivot)

(defcustom pivot-default-session "main"
  "Default session name."
  :type 'string)

(defcustom pivot-tab-label-padding " "
  "Horizontal padding added around each visible tab label."
  :type 'string)

(defcustom pivot-tab-group-label-padding " "
  "Horizontal padding added around the visible session label."
  :type 'string)

(defcustom pivot-tab-bar-auto-width nil
  "Whether `pivot-mode' should keep `tab-bar-auto-width' enabled.
When nil, tab widths follow the displayed label width instead of being
stretched across the entire tab bar."
  :type 'boolean)

(defvar pivot--saved-tab-bar-tab-group-function nil
  "Original value of `tab-bar-tab-group-function'
before enabling `pivot-mode'.")

(defvar pivot--saved-tab-bar-mode nil
  "Original value of `tab-bar-mode' before enabling `pivot-mode'.")

(defvar pivot--saved-tab-bar-format nil
  "Original value of `tab-bar-format' before enabling `pivot-mode'.")

(defvar pivot--saved-tab-bar-show-inactive-group-tabs nil
  "Original value of `tab-bar-show-inactive-group-tabs' before enabling mode.")

(defvar pivot--saved-tab-bar-close-button-show nil
  "Original value of `tab-bar-close-button-show' before enabling mode.")

(defvar pivot--saved-tab-bar-auto-width nil
  "Original value of `tab-bar-auto-width' before enabling mode.")

(defconst pivot--selector-key-preference
  (string-to-list "asdfjkl;ghwertyuiopcvbnmzqx1234567890")
  "Preferred key order for quick session selection on a QWERTY layout.")

(defvar pivot-session-hotkeys nil
  "Alist mapping hotkey characters to session names.")

(defvar pivot--last-session nil
  "Previously active session name.")

(defvar pivot--inhibit-command-scoping nil
  "When non-nil, bypass session-scoped tab-bar command advice.")

;;; Core helpers

(defun pivot--tab-group (tab)
  "Return the session group for TAB."
  (or (alist-get 'group tab)
      pivot-default-session))

(defun pivot--current ()
  "Return the current session name."
  (pivot--tab-group (tab-bar--current-tab)))

(defun pivot--record-session-transition (from to)
  "Record a session transition from FROM to TO."
  (when (and from to (not (equal from to)))
    (setq pivot--last-session from)))

(defun pivot--all-tabs (&optional frame)
  "Return the complete tab list for FRAME."
  (tab-bar-tabs frame))

(defun pivot--current-tab-in-list (tabs)
  "Return the current tab object from TABS."
  (seq-find (lambda (tab)
              (eq (car tab) 'current-tab))
            tabs))

(defun pivot--tabs-in-current-session (&optional frame)
  "Return tabs in the current session for FRAME."
  (let* ((tabs (pivot--all-tabs frame))
         (current-tab (pivot--current-tab-in-list tabs))
         (current-group (and current-tab
                             (pivot--tab-group current-tab))))
    (if current-group
        (seq-filter
         (lambda (tab)
           (equal (pivot--tab-group tab) current-group))
         tabs)
      tabs)))

(defun pivot--call-unscoped (fn &rest args)
  "Call FN with ARGS while bypassing session-scoped advice."
  (let ((pivot--inhibit-command-scoping t))
    (apply fn args)))

(defun pivot--set (name)
  "Assign current tab to session NAME."
  (tab-bar-change-tab-group name))

(defun pivot--tabs (&optional frame)
  "Return all tabs for FRAME."
  (pivot--all-tabs frame))

(defun pivot--ensure-current-session ()
  "Assign the current tab to the default session if it has no group."
  (unless (alist-get 'group (tab-bar--current-tab))
    (pivot--set pivot-default-session)))

(defun pivot--select-tab (tab)
  "Select TAB from the complete tab list."
  (let* ((current-session (pivot--current))
         (tabs (pivot--all-tabs))
         (index (seq-position tabs tab #'eq)))
    (when index
      (pivot--record-session-transition
       current-session
       (pivot--tab-group tab))
      (pivot--call-unscoped #'tab-bar-select-tab (1+ index)))))

(defun pivot--switch-tab-in-current-session (arg)
  "Switch ARG tabs within the current session."
  (let* ((tabs (pivot--tabs-in-current-session))
         (current-tab (pivot--current-tab-in-list (pivot--all-tabs)))
         (current-index (seq-position tabs current-tab #'eq)))
    (when (and current-index tabs)
      (pivot--select-tab
       (nth (mod (+ current-index arg) (length tabs))
            tabs)))))

(defun pivot--move-tab-in-current-session (arg)
  "Move the current tab ARG positions within the current session."
  (let* ((tabs (pivot--tabs-in-current-session))
         (current-tab (pivot--current-tab-in-list (pivot--all-tabs)))
         (current-index (seq-position tabs current-tab #'eq)))
    (when (and current-index (> (length tabs) 1))
      (let* ((target-index (max 0 (min (+ current-index arg)
                                       (1- (length tabs)))))
             (target-tab (nth target-index tabs))
             (target-position (seq-position (pivot--all-tabs) target-tab #'eq)))
        (when (and target-position (/= current-index target-index))
          (pivot--call-unscoped #'tab-bar-move-tab-to (1+ target-position)))))))

(defun pivot--advice-switch-to-next-tab (orig &optional arg)
  "Restrict `tab-bar-switch-to-next-tab' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (funcall orig arg)
    (pivot--switch-tab-in-current-session (or arg 1))))

(defun pivot--advice-switch-to-prev-tab (orig &optional arg)
  "Restrict `tab-bar-switch-to-prev-tab' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (funcall orig arg)
    (pivot--switch-tab-in-current-session (- (or arg 1)))))

(defun pivot--advice-select-tab (orig tab-number &rest args)
  "Restrict `tab-bar-select-tab' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (apply orig tab-number args)
    (let* ((frame (car args))
           (tab (nth (1- tab-number)
                     (pivot--tabs-in-current-session frame))))
      (unless tab
        (user-error "No such tab in current session: %s" tab-number))
      (pivot--select-tab tab))))

(defun pivot--advice-move-tab (orig &optional arg)
  "Restrict `tab-bar-move-tab' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (funcall orig arg)
    (pivot--move-tab-in-current-session (or arg 1))))

(defun pivot--advice-move-tab-to (orig to-position &rest args)
  "Restrict `tab-bar-move-tab-to' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (apply orig to-position args)
    (let* ((tabs (pivot--tabs-in-current-session))
           (target-index (max 0 (min (1- to-position) (1- (length tabs)))))
           (target-tab (nth target-index tabs))
           (current-tab (pivot--current-tab-in-list (pivot--all-tabs)))
           (target-position (and target-tab
                                 (seq-position (pivot--all-tabs) target-tab #'eq))))
      (when (and target-position
                 (not (eq target-tab current-tab)))
        (apply #'pivot--call-unscoped
               #'tab-bar-move-tab-to
               (1+ target-position)
               args)))))

(defun pivot--tab-bar-format (format)
  "Return FORMAT adjusted for `pivot-mode'."
  (seq-remove
   (lambda (item)
     (eq item 'tab-bar-format-add-tab))
   (mapcar (lambda (item)
             (if (memq item '(tab-bar-format-tabs tab-bar-format-tabs-groups))
                 'pivot--format-tabs
               item))
           format)))

(defun pivot--pad-tab (tab)
  "Return TAB with padding applied to its displayed name."
  (let ((tab (copy-tree tab)))
    (when-let* ((name (alist-get 'name tab)))
      (setf (alist-get 'name tab)
            (pivot--pad-label name pivot-tab-label-padding)))
    tab))

(defun pivot--pad-label (label padding)
  "Return LABEL with PADDING added to both sides."
  (concat padding label padding))

(defun pivot--format-current-tab-group (tab)
  "Format TAB's current session label with pivot padding."
  (let ((items (tab-bar--format-tab-group tab 1 t)))
    (mapcar
     (lambda (item)
       (if (and (eq (nth 1 item) 'menu-item)
                (stringp (nth 2 item))
                (eq (car item) 'current-group))
           (let ((item (copy-sequence item)))
             (setf (nth 2 item)
                   (pivot--pad-label
                    (nth 2 item)
                    pivot-tab-group-label-padding))
             item)
         item))
     items)))

(defun pivot--format-tabs ()
  "Produce tab-bar items for the current session only."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-tab (tab-bar--current-tab-find tabs))
         (current-group (and current-tab
                             (funcall tab-bar-tab-group-function current-tab)))
         (visible-tabs
          (seq-filter
           (lambda (tab)
             (equal (funcall tab-bar-tab-group-function tab) current-group))
           tabs))
         (i 0))
    (append
     (when (and current-tab current-group)
       (pivot--format-current-tab-group current-tab))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (let ((tab-bar-tab-face-function tab-bar-tab-group-face-function))
          (tab-bar--format-tab (pivot--pad-tab tab) i)))
      visible-tabs))))

(defun pivot--sessions ()
  "Return all session names."
  (delete-dups
   (mapcar #'pivot--tab-group
           (pivot--all-tabs))))

(defun pivot--tabs-in-session (name)
  (seq-filter
   (lambda (tab)
     (equal (pivot--tab-group tab) name))
   (pivot--all-tabs)))

(defun pivot--preferred-tab (name)
  "Return the preferred tab to select for session NAME."
  (car (seq-sort-by
        (lambda (tab)
          (or (alist-get 'time tab) 0))
        #'>
        (pivot--tabs-in-session name))))

(defun pivot--fallback-session (&optional excluded-name)
  "Return a surviving session name, excluding EXCLUDED-NAME when possible."
  (car (seq-remove (lambda (name)
                     (equal name excluded-name))
                   (pivot--sorted-sessions))))

(defun pivot--sorted-sessions ()
  "Return session names sorted alphabetically."
  (sort (copy-sequence (pivot--sessions)) #'string-lessp))

(defun pivot--format-menu (title entries)
  "Return a formatted menu with TITLE and ENTRIES.

ENTRIES should already include any text properties to render."
  (let* ((entry-width (max 1 (apply #'max (mapcar #'string-width entries))))
         (rows (min 6 (length entries)))
         (columns (max 1 (ceiling (/ (float (length entries)) rows))))
         lines)
    (dotimes (row rows)
      (let (line-parts)
        (dotimes (column columns)
          (let ((index (+ row (* column rows))))
            (when-let* ((entry (nth index entries)))
              (push (truncate-string-to-width entry entry-width 0 ?\s t)
                    line-parts))))
        (push (string-join (nreverse line-parts)
                           (make-string 4 ?\s))
              lines)))
    (concat
     (propertize (concat title "\n") 'face 'minibuffer-prompt)
     (string-join (nreverse lines) "\n"))))

(defun pivot--session-hotkey (name)
  "Return the hotkey assigned to session NAME, or nil."
  (car (rassoc name pivot-session-hotkeys)))

(defun pivot--hotkey-session (key)
  "Return the session assigned to hotkey KEY, or nil."
  (alist-get key pivot-session-hotkeys))

(defun pivot--clear-session-hotkey (name)
  "Remove any hotkey bound to session NAME."
  (setq pivot-session-hotkeys
        (cl-remove name pivot-session-hotkeys
                   :key #'cdr
                   :test #'equal)))

(defun pivot--assign-hotkey (key name)
  "Assign hotkey KEY to session NAME."
  (let ((existing (pivot--hotkey-session key)))
    (when (and existing
               (not (equal existing name)))
      (user-error "Hotkey [%s] is already assigned to %s"
                  (single-key-description key)
                  existing)))
  (pivot--clear-session-hotkey name)
  (setf (alist-get key pivot-session-hotkeys) name)
  key)

(defun pivot--rename-hotkey-session (old-name new-name)
  "Update hotkey bindings from OLD-NAME to NEW-NAME."
  (let ((key (pivot--session-hotkey old-name)))
    (when key
      (setf (alist-get key pivot-session-hotkeys) new-name))))

(defun pivot--rename-last-session (old-name new-name)
  "Update last-session tracking from OLD-NAME to NEW-NAME."
  (when (equal pivot--last-session old-name)
    (setq pivot--last-session new-name)))

(defun pivot--hotkey-candidates ()
  "Return bound hotkey candidates sorted alphabetically by key label.

Each item has the form (KEY SESSION LABEL)."
  (mapcar
   (lambda (binding)
     (let ((key (car binding))
           (session (cdr binding)))
       (list key
             session
             (concat
              (propertize (format "[%s]" (single-key-description key))
                          'face 'pivot-quick-select-key)
              " "
              (propertize session
                          'face 'pivot-quick-select-session)))))
   (sort (copy-sequence pivot-session-hotkeys)
         (lambda (left right)
           (string-lessp (single-key-description (car left))
                         (single-key-description (car right)))))))

(defun pivot--bound-hotkey-candidates ()
  "Return bound hotkey candidates."
  (pivot--hotkey-candidates))

(defun pivot--hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a session hotkey."
  (let ((entries (mapcar #'cl-third (pivot--hotkey-candidates))))
    (if entries
        (pivot--format-menu title entries)
      (concat
       (propertize (concat title "\n") 'face 'minibuffer-prompt)
       "No hotkeys assigned"))))

(defun pivot--bound-hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a bound hotkey."
  (pivot--format-menu
   title
   (mapcar #'cl-third (pivot--bound-hotkey-candidates))))

(defun pivot-read-hotkey (&optional title)
  "Prompt for a hotkey using `read-key' with TITLE."
  (read-key (pivot--hotkey-prompt (or title "Select hotkey"))))

(defun pivot-read-bound-hotkey (&optional title)
  "Prompt for a bound hotkey using `read-key' with TITLE."
  (let ((candidates (pivot--bound-hotkey-candidates)))
    (unless candidates
      (user-error "No session hotkeys are assigned"))
    (read-key (pivot--bound-hotkey-prompt
               (or title "Jump to session:")))))

(defun pivot--read-available-hotkey (name)
  "Prompt until an available hotkey is chosen for NAME."
  (let ((key (pivot-read-hotkey
              (format "Assign hotkey to %s" name))))
    (if-let* ((existing (pivot--hotkey-session key)))
        (if (equal existing name)
            key
          (message "Hotkey [%s] is already assigned to %s. Choose another."
                   (single-key-description key)
                   existing)
          (sit-for 1)
          (pivot--read-available-hotkey name))
      key)))

(defun pivot--rename-session-tabs (old-name new-name)
  "Rename OLD-NAME session tabs to NEW-NAME."
  (let ((current-tab (tab-bar--current-tab)))
    (dolist (tab (pivot--tabs-in-session old-name))
      (pivot--select-tab tab)
      (tab-bar-change-tab-group new-name))
    (pivot--select-tab current-tab)))

(defun pivot--session-selector-candidates ()
  "Return session candidates with single-key selectors.

Each item has the form (KEY NAME LABEL). KEY is the character
accepted by `read-key', NAME is the session name, and LABEL is the
string shown in the prompt."
  (let ((sessions (pivot--sorted-sessions))
        candidates)
    (when (> (length sessions) (length pivot--selector-key-preference))
      (user-error "Too many sessions for single-key selection"))
    (cl-loop
     for name in sessions
     for key in pivot--selector-key-preference
     do (push (list key
                    name
                    (concat
                     (propertize (format "[%c]" key)
                                 'face 'pivot-quick-select-key)
                     " "
                     (propertize name
                                 'face 'pivot-quick-select-session)))
              candidates))
    (nreverse candidates)))

(defun pivot--quick-select-prompt ()
  "Return a formatted prompt for quick session selection."
  (pivot--format-menu
   "Select session:"
   (mapcar #'cl-third (pivot--session-selector-candidates))))

(defun pivot--quick-select-session ()
  "Prompt for a session using a single key press."
  (let* ((candidates (pivot--session-selector-candidates))
         (prompt (pivot--quick-select-prompt))
         (key (read-key prompt))
         (selection (seq-find (lambda (candidate)
                                (= (car candidate) key))
                              candidates)))
    (unless selection
      (user-error "No session is bound to %s" (single-key-description key)))
    (cadr selection)))

(defun pivot--handle-tab-open (tab)
  "Initialize TAB after opening."
  (unless (alist-get 'group tab)
    (setf (alist-get 'group tab) pivot-default-session)))

(defun pivot--cleanup-removed-sessions (before-sessions)
  "Clear state for sessions in BEFORE-SESSIONS that no longer exist."
  (dolist (name before-sessions)
    (unless (member name (pivot--sessions))
      (pivot--clear-session-hotkey name)
      (when (equal pivot--last-session name)
        (setq pivot--last-session nil)))))

(defun pivot--advice-close-tab (orig &rest args)
  "Keep session state consistent around `tab-bar-close-tab' ORIG with ARGS."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (apply orig args)
    (let* ((before-sessions (pivot--sessions))
           (current-session (pivot--current))
           (last-tab-in-session-p (= (length (pivot--tabs-in-current-session)) 1))
           (fallback-session (and last-tab-in-session-p
                                  (pivot--fallback-session current-session))))
      (prog1
          ;; `tab-bar-close-tab' may re-select a surviving tab by its global
          ;; tab-bar index while tearing down the closed tab. Bypass
          ;; session-scoped advice for that internal selection.
          (apply #'pivot--call-unscoped orig args)
        (pivot--cleanup-removed-sessions before-sessions)
        (when (and fallback-session
                   (not (member current-session (pivot--sessions)))
                   (member fallback-session (pivot--sessions)))
          (pivot-switch fallback-session))))))

(defun pivot--advice-close-other-tabs (orig &optional tab-number)
  "Restrict `tab-bar-close-other-tabs' ORIG to the current session."
  (if (or pivot--inhibit-command-scoping
          (not pivot-mode))
      (funcall orig tab-number)
    (let* ((tabs (pivot--tabs-in-current-session))
           (target-tab (if tab-number
                           (nth (1- tab-number) tabs)
                         (pivot--current-tab-in-list (pivot--all-tabs)))))
      (unless target-tab
        (user-error "No such tab in current session: %s" tab-number))
      (pivot--select-tab target-tab)
      (dolist (tab (delq target-tab (copy-sequence tabs)))
        (when-let* ((position (seq-position (pivot--all-tabs) tab #'eq)))
          (pivot--call-unscoped #'tab-bar-close-tab (1+ position)))))))

;;; Sparse keymap (unbound, ready for future use)

(defvar pivot-keymap
  (let ((map (make-sparse-keymap)))
    ;; Example commands, ready to bind later if desired
    (define-key map (kbd "s") #'pivot-switch)
    (define-key map (kbd "S") #'pivot-switch-completing)
    (define-key map (kbd "l") #'pivot-switch-last)
    (define-key map (kbd "a") #'pivot-assign-hotkey)
    (define-key map (kbd "r") #'pivot-rename)
    (define-key map (kbd "j") #'pivot-jump-hotkey)
    (define-key map (kbd "n") #'pivot-new)
    (define-key map (kbd "k") #'pivot-kill)
    map)
  "Sparse keymap for `pivot-mode` commands.
Currently not bound to any prefix, ready for future keybindings.")

;;; Completing read candidates

(defun pivot--session-annotation (candidate)
  "Return completion annotation for session CANDIDATE."
  (if-let* ((key (pivot--session-hotkey candidate)))
      (format " [%s]" (single-key-description key))
    ""))

(defun pivot-read (&optional prompt)
  "Prompt for a session using `completing-read` with PROMPT."
  (let ((completion-extra-properties
         '(:category pivot-session
           :annotation-function pivot--session-annotation)))
    (completing-read (or prompt "Session: ")
                     (pivot--sessions)
                     nil t)))

(with-eval-after-load 'marginalia
  (defun marginalia-annotate-pivot-session (candidate)
    "Annotate pivot CANDIDATE with its assigned hotkey."
    (pivot--session-annotation candidate))
  (add-to-list 'marginalia-annotators
               '(pivot-session
                 marginalia-annotate-pivot-session
                 builtin
                 none)))

(defun pivot--read-switch-session ()
  "Read a session name for `pivot-switch'."
  (let ((sessions (pivot--sessions)))
    (cond
     ((null sessions)
      (user-error "No sessions available"))
     ((= (length sessions) 1)
      (car sessions))
     (t
      (pivot--quick-select-session)))))

;;; Commands

(defun pivot-switch-by-name (name)
  "Switch to session NAME.

This is the public entry point for switching sessions directly by
name."
  (interactive (list (pivot-read)))
  (unless (member name (pivot--sessions))
    (user-error "No such session: %s" name))
  (unless (equal name (pivot--current))
    (let ((tab (pivot--preferred-tab name)))
      (when tab
        (pivot--select-tab tab)))))

(defun pivot-switch (name)
  "Switch to session NAME."
  (interactive (list (pivot--read-switch-session)))
  (pivot-switch-by-name name))

(defun pivot-switch-completing (name)
  "Switch to session NAME using minibuffer completion."
  (interactive (list (pivot-read)))
  (pivot-switch name))

(defun pivot-switch-last ()
  "Switch to the previously active session."
  (interactive)
  (unless pivot--last-session
    (user-error "No previous session"))
  (unless (member pivot--last-session (pivot--sessions))
    (user-error "Previous session no longer exists"))
  (pivot-switch pivot--last-session))

(defun pivot-assign-hotkey (name key)
  "Assign hotkey KEY to session NAME."
  (interactive
   (let ((name (pivot-read)))
     (list name
           (pivot--read-available-hotkey name))))
  (setq key (pivot--assign-hotkey key name))
  (message "Bound session %s to [%s]" name (single-key-description key)))

(defun pivot-jump-hotkey (key)
  "Jump to the session bound to hotkey KEY."
  (interactive (list (pivot-read-bound-hotkey "Jump to session:")))
  (if-let* ((session (pivot--hotkey-session key)))
      (pivot-switch session)
    (message "No session is bound to [%s]" (single-key-description key))))

(defun pivot-new (name)
  "Create new session NAME."
  (interactive (list (read-string "New session: ")))
  (tab-bar-new-tab)
  (pivot--set name))

(defun pivot-rename (old-name new-name)
  "Rename session OLD-NAME to NEW-NAME."
  (interactive
   (let ((old-name (pivot-read)))
     (list old-name
           (read-string (format "Rename session %s to: " old-name)
                        old-name))))
  (when (string-empty-p new-name)
    (user-error "Session name cannot be empty"))
  (unless (member old-name (pivot--sessions))
    (user-error "No such session: %s" old-name))
  (unless (or (equal old-name new-name)
              (not (member new-name (pivot--sessions))))
    (user-error "Session already exists: %s" new-name))
  (unless (equal old-name new-name)
    (pivot--rename-session-tabs old-name new-name)
    (pivot--rename-hotkey-session old-name new-name)
    (pivot--rename-last-session old-name new-name)))

(defun pivot-kill (name)
  "Kill session NAME."
  (interactive (list (pivot-read)))
  (when (= (length (pivot--sessions)) 1)
    (user-error "Cannot kill the last session"))
  (pivot--clear-session-hotkey name)
  (when (equal pivot--last-session name)
    (setq pivot--last-session nil))
  (dolist (tab (pivot--tabs-in-session name))
    (tab-bar-close-tab (1+ (seq-position (pivot--all-tabs) tab #'eq)))))

;;; Minor mode

(define-minor-mode pivot-mode
  "Manage sessions using tab-bar groups, with mode line display."
  :global t
  ;; :keymap pivot-keymap
  (if pivot-mode
      (progn
        (setq pivot--saved-tab-bar-mode tab-bar-mode)
        (tab-bar-mode 1)
        (setq pivot--saved-tab-bar-tab-group-function tab-bar-tab-group-function)
        (setq pivot--saved-tab-bar-format tab-bar-format)
        (setq pivot--saved-tab-bar-show-inactive-group-tabs
              tab-bar-show-inactive-group-tabs)
        (setq pivot--saved-tab-bar-close-button-show
              tab-bar-close-button-show)
        (setq pivot--saved-tab-bar-auto-width
              tab-bar-auto-width)
        (setq tab-bar-tab-group-function #'pivot--tab-group)
        (setq tab-bar-format
              (pivot--tab-bar-format tab-bar-format))
        (setq tab-bar-show-inactive-group-tabs nil)
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-auto-width pivot-tab-bar-auto-width)
        (advice-add 'tab-next :around
                    #'pivot--advice-switch-to-next-tab)
        (advice-add 'tab-bar-switch-to-next-tab :around
                    #'pivot--advice-switch-to-next-tab)
        (advice-add 'tab-previous :around
                    #'pivot--advice-switch-to-prev-tab)
        (advice-add 'tab-bar-switch-to-prev-tab :around
                    #'pivot--advice-switch-to-prev-tab)
        (advice-add 'tab-bar-select-tab :around
                    #'pivot--advice-select-tab)
        (advice-add 'tab-bar-close-tab :around
                    #'pivot--advice-close-tab)
        (advice-add 'tab-bar-close-other-tabs :around
                    #'pivot--advice-close-other-tabs)
        (advice-add 'tab-move :around
                    #'pivot--advice-move-tab)
        (advice-add 'tab-bar-move-tab :around
                    #'pivot--advice-move-tab)
        (advice-add 'tab-bar-move-tab-to :around
                    #'pivot--advice-move-tab-to)
        (add-hook 'tab-bar-tab-post-open-functions
                  #'pivot--handle-tab-open)
        ;; Ensure startup tab has a session
        (pivot--ensure-current-session))
    ;; Disable
    (advice-remove 'tab-next
                   #'pivot--advice-switch-to-next-tab)
    (advice-remove 'tab-bar-switch-to-next-tab
                   #'pivot--advice-switch-to-next-tab)
    (advice-remove 'tab-previous
                   #'pivot--advice-switch-to-prev-tab)
    (advice-remove 'tab-bar-switch-to-prev-tab
                   #'pivot--advice-switch-to-prev-tab)
    (advice-remove 'tab-bar-select-tab
                   #'pivot--advice-select-tab)
    (advice-remove 'tab-bar-close-tab
                   #'pivot--advice-close-tab)
    (advice-remove 'tab-bar-close-other-tabs
                   #'pivot--advice-close-other-tabs)
    (advice-remove 'tab-move
                   #'pivot--advice-move-tab)
    (advice-remove 'tab-bar-move-tab
                   #'pivot--advice-move-tab)
    (advice-remove 'tab-bar-move-tab-to
                   #'pivot--advice-move-tab-to)
    (remove-hook 'tab-bar-tab-post-open-functions
                 #'pivot--handle-tab-open)
    (setq tab-bar-tab-group-function pivot--saved-tab-bar-tab-group-function)
    (setq tab-bar-format pivot--saved-tab-bar-format)
    (setq tab-bar-show-inactive-group-tabs
          pivot--saved-tab-bar-show-inactive-group-tabs)
    (setq tab-bar-close-button-show
          pivot--saved-tab-bar-close-button-show)
    (setq tab-bar-auto-width
          pivot--saved-tab-bar-auto-width)
    (when (null pivot--saved-tab-bar-mode)
      (tab-bar-mode 0))
    (setq pivot--saved-tab-bar-mode nil)
    (setq pivot--saved-tab-bar-tab-group-function nil)
    (setq pivot--saved-tab-bar-format nil)
    (setq pivot--saved-tab-bar-show-inactive-group-tabs nil)
    (setq pivot--saved-tab-bar-close-button-show nil)
    (setq pivot--saved-tab-bar-auto-width nil)))

;;; Startup safety

(add-hook 'emacs-startup-hook
          (lambda ()
            (when pivot-mode
              (pivot--ensure-current-session))))

(provide 'pivot)
;;; pivot.el ends here
