;;; tab-clique.el --- tmux-like sessions using tab-bar groups -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "31.0"))

(require 'cl-lib)
(require 'subr-x)
(require 'tab-bar)
(require 'seq)

;;; Customization

(defgroup tab-clique nil
  "Session management using tab-bar groups."
  :group 'convenience)

(defface tab-clique-quick-select-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for keys in the quick session selector."
  :group 'tab-clique)

(defface tab-clique-quick-select-session
  '((t :inherit default))
  "Face used for session names in the quick session selector."
  :group 'tab-clique)

(defcustom tab-clique-default-session "main"
  "Default session name."
  :type 'string)

(defcustom tab-clique-tab-label-padding " "
  "Horizontal padding added around each visible tab label."
  :type 'string)

(defcustom tab-clique-tab-group-label-padding " "
  "Horizontal padding added around the visible session label."
  :type 'string)

(defcustom tab-clique-tab-bar-auto-width nil
  "Whether `tab-clique-mode' should keep `tab-bar-auto-width' enabled.
When nil, tab widths follow the displayed label width instead of being
stretched across the entire tab bar."
  :type 'boolean)

(defvar tab-clique--saved-tab-bar-tab-group-function nil
  "Original value of `tab-bar-tab-group-function'
before enabling `tab-clique-mode'.")

(defvar tab-clique--saved-tab-bar-mode nil
  "Original value of `tab-bar-mode' before enabling `tab-clique-mode'.")

(defvar tab-clique--saved-tab-bar-format nil
  "Original value of `tab-bar-format' before enabling `tab-clique-mode'.")

(defvar tab-clique--saved-tab-bar-show-inactive-group-tabs nil
  "Original value of `tab-bar-show-inactive-group-tabs' before enabling mode.")

(defvar tab-clique--saved-tab-bar-close-button-show nil
  "Original value of `tab-bar-close-button-show' before enabling mode.")

(defvar tab-clique--saved-tab-bar-auto-width nil
  "Original value of `tab-bar-auto-width' before enabling mode.")

(defconst tab-clique--selector-key-preference
  (string-to-list "asdfjkl;ghwertyuiopcvbnmzqx1234567890")
  "Preferred key order for quick session selection on a QWERTY layout.")

(defvar tab-clique-session-hotkeys nil
  "Alist mapping hotkey characters to session names.")

(defvar tab-clique--last-session nil
  "Previously active session name.")

(defvar tab-clique--inhibit-command-scoping nil
  "When non-nil, bypass session-scoped tab-bar command advice.")

;;; Core helpers

(defun tab-clique--tab-group (tab)
  "Return the session group for TAB."
  (or (alist-get 'group tab)
      tab-clique-default-session))

(defun tab-clique--current ()
  "Return the current session name."
  (tab-clique--tab-group (tab-bar--current-tab)))

(defun tab-clique--record-session-transition (from to)
  "Record a session transition from FROM to TO."
  (when (and from to (not (equal from to)))
    (setq tab-clique--last-session from)))

(defun tab-clique--all-tabs (&optional frame)
  "Return the complete tab list for FRAME."
  (tab-bar-tabs frame))

(defun tab-clique--current-tab-in-list (tabs)
  "Return the current tab object from TABS."
  (seq-find (lambda (tab)
              (eq (car tab) 'current-tab))
            tabs))

(defun tab-clique--tabs-in-current-session (&optional frame)
  "Return tabs in the current session for FRAME."
  (let* ((tabs (tab-clique--all-tabs frame))
         (current-tab (tab-clique--current-tab-in-list tabs))
         (current-group (and current-tab
                             (tab-clique--tab-group current-tab))))
    (if current-group
        (seq-filter
         (lambda (tab)
           (equal (tab-clique--tab-group tab) current-group))
         tabs)
      tabs)))

(defun tab-clique--call-unscoped (fn &rest args)
  "Call FN with ARGS while bypassing session-scoped advice."
  (let ((tab-clique--inhibit-command-scoping t))
    (apply fn args)))

(defun tab-clique--set (name)
  "Assign current tab to session NAME."
  (tab-bar-change-tab-group name))

(defun tab-clique--tabs (&optional frame)
  "Return all tabs for FRAME."
  (tab-clique--all-tabs frame))

(defun tab-clique--ensure-current-session ()
  "Assign the current tab to the default session if it has no group."
  (unless (alist-get 'group (tab-bar--current-tab))
    (tab-clique--set tab-clique-default-session)))

(defun tab-clique--select-tab (tab)
  "Select TAB from the complete tab list."
  (let* ((current-session (tab-clique--current))
         (tabs (tab-clique--all-tabs))
         (index (seq-position tabs tab #'eq)))
    (when index
      (tab-clique--record-session-transition
       current-session
       (tab-clique--tab-group tab))
      (tab-clique--call-unscoped #'tab-bar-select-tab (1+ index)))))

(defun tab-clique--switch-tab-in-current-session (arg)
  "Switch ARG tabs within the current session."
  (let* ((tabs (tab-clique--tabs-in-current-session))
         (current-tab (tab-clique--current-tab-in-list (tab-clique--all-tabs)))
         (current-index (seq-position tabs current-tab #'eq)))
    (when (and current-index tabs)
      (tab-clique--select-tab
       (nth (mod (+ current-index arg) (length tabs))
            tabs)))))

(defun tab-clique--move-tab-in-current-session (arg)
  "Move the current tab ARG positions within the current session."
  (let* ((tabs (tab-clique--tabs-in-current-session))
         (current-tab (tab-clique--current-tab-in-list (tab-clique--all-tabs)))
         (current-index (seq-position tabs current-tab #'eq)))
    (when (and current-index (> (length tabs) 1))
      (let* ((target-index (max 0 (min (+ current-index arg)
                                       (1- (length tabs)))))
             (target-tab (nth target-index tabs))
             (target-position (seq-position (tab-clique--all-tabs) target-tab #'eq)))
        (when (and target-position (/= current-index target-index))
          (tab-clique--call-unscoped #'tab-bar-move-tab-to (1+ target-position)))))))

(defun tab-clique--advice-switch-to-next-tab (orig &optional arg)
  "Restrict `tab-bar-switch-to-next-tab' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (funcall orig arg)
    (tab-clique--switch-tab-in-current-session (or arg 1))))

(defun tab-clique--advice-switch-to-prev-tab (orig &optional arg)
  "Restrict `tab-bar-switch-to-prev-tab' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (funcall orig arg)
    (tab-clique--switch-tab-in-current-session (- (or arg 1)))))

(defun tab-clique--advice-select-tab (orig tab-number &rest args)
  "Restrict `tab-bar-select-tab' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (apply orig tab-number args)
    (let* ((frame (car args))
           (tab (nth (1- tab-number)
                     (tab-clique--tabs-in-current-session frame))))
      (unless tab
        (user-error "No such tab in current session: %s" tab-number))
      (tab-clique--select-tab tab))))

(defun tab-clique--advice-move-tab (orig &optional arg)
  "Restrict `tab-bar-move-tab' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (funcall orig arg)
    (tab-clique--move-tab-in-current-session (or arg 1))))

(defun tab-clique--advice-move-tab-to (orig to-position &rest args)
  "Restrict `tab-bar-move-tab-to' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (apply orig to-position args)
    (let* ((tabs (tab-clique--tabs-in-current-session))
           (target-index (max 0 (min (1- to-position) (1- (length tabs)))))
           (target-tab (nth target-index tabs))
           (current-tab (tab-clique--current-tab-in-list (tab-clique--all-tabs)))
           (target-position (and target-tab
                                 (seq-position (tab-clique--all-tabs) target-tab #'eq))))
      (when (and target-position
                 (not (eq target-tab current-tab)))
        (apply #'tab-clique--call-unscoped
               #'tab-bar-move-tab-to
               (1+ target-position)
               args)))))

(defun tab-clique--tab-bar-format (format)
  "Return FORMAT adjusted for `tab-clique-mode'."
  (seq-remove
   (lambda (item)
     (eq item 'tab-bar-format-add-tab))
   (mapcar (lambda (item)
             (if (memq item '(tab-bar-format-tabs tab-bar-format-tabs-groups))
                 'tab-clique--format-tabs
               item))
           format)))

(defun tab-clique--pad-tab (tab)
  "Return TAB with padding applied to its displayed name."
  (let ((tab (copy-tree tab)))
    (when-let* ((name (alist-get 'name tab)))
      (setf (alist-get 'name tab)
            (tab-clique--pad-label name tab-clique-tab-label-padding)))
    tab))

(defun tab-clique--pad-label (label padding)
  "Return LABEL with PADDING added to both sides."
  (concat padding label padding))

(defun tab-clique--format-current-tab-group (tab)
  "Format TAB's current session label with tab-clique padding."
  (let ((items (tab-bar--format-tab-group tab 1 t)))
    (mapcar
     (lambda (item)
       (if (and (eq (nth 1 item) 'menu-item)
                (stringp (nth 2 item))
                (eq (car item) 'current-group))
           (let ((item (copy-sequence item)))
             (setf (nth 2 item)
                   (tab-clique--pad-label
                    (nth 2 item)
                    tab-clique-tab-group-label-padding))
             item)
         item))
     items)))

(defun tab-clique--format-tabs ()
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
       (tab-clique--format-current-tab-group current-tab))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (let ((tab-bar-tab-face-function tab-bar-tab-group-face-function))
          (tab-bar--format-tab (tab-clique--pad-tab tab) i)))
      visible-tabs))))

(defun tab-clique--sessions ()
  "Return all session names."
  (delete-dups
   (mapcar #'tab-clique--tab-group
           (tab-clique--all-tabs))))

(defun tab-clique--tabs-in-session (name)
  (seq-filter
   (lambda (tab)
     (equal (tab-clique--tab-group tab) name))
   (tab-clique--all-tabs)))

(defun tab-clique--preferred-tab (name)
  "Return the preferred tab to select for session NAME."
  (car (seq-sort-by
        (lambda (tab)
          (or (alist-get 'time tab) 0))
        #'>
        (tab-clique--tabs-in-session name))))

(defun tab-clique--fallback-session (&optional excluded-name)
  "Return a surviving session name, excluding EXCLUDED-NAME when possible."
  (car (seq-remove (lambda (name)
                     (equal name excluded-name))
                   (tab-clique--sorted-sessions))))

(defun tab-clique--sorted-sessions ()
  "Return session names sorted alphabetically."
  (sort (copy-sequence (tab-clique--sessions)) #'string-lessp))

(defun tab-clique--format-menu (title entries)
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

(defun tab-clique--session-hotkey (name)
  "Return the hotkey assigned to session NAME, or nil."
  (car (rassoc name tab-clique-session-hotkeys)))

(defun tab-clique--hotkey-session (key)
  "Return the session assigned to hotkey KEY, or nil."
  (alist-get key tab-clique-session-hotkeys))

(defun tab-clique--clear-session-hotkey (name)
  "Remove any hotkey bound to session NAME."
  (setq tab-clique-session-hotkeys
        (cl-remove name tab-clique-session-hotkeys
                   :key #'cdr
                   :test #'equal)))

(defun tab-clique--assign-hotkey (key name)
  "Assign hotkey KEY to session NAME."
  (let ((existing (tab-clique--hotkey-session key)))
    (when (and existing
               (not (equal existing name)))
      (user-error "Hotkey [%s] is already assigned to %s"
                  (single-key-description key)
                  existing)))
  (tab-clique--clear-session-hotkey name)
  (setf (alist-get key tab-clique-session-hotkeys) name)
  key)

(defun tab-clique--rename-hotkey-session (old-name new-name)
  "Update hotkey bindings from OLD-NAME to NEW-NAME."
  (let ((key (tab-clique--session-hotkey old-name)))
    (when key
      (setf (alist-get key tab-clique-session-hotkeys) new-name))))

(defun tab-clique--rename-last-session (old-name new-name)
  "Update last-session tracking from OLD-NAME to NEW-NAME."
  (when (equal tab-clique--last-session old-name)
    (setq tab-clique--last-session new-name)))

(defun tab-clique--hotkey-candidates ()
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
                          'face 'tab-clique-quick-select-key)
              " "
              (propertize session
                          'face 'tab-clique-quick-select-session)))))
   (sort (copy-sequence tab-clique-session-hotkeys)
         (lambda (left right)
           (string-lessp (single-key-description (car left))
                         (single-key-description (car right)))))))

(defun tab-clique--bound-hotkey-candidates ()
  "Return bound hotkey candidates."
  (tab-clique--hotkey-candidates))

(defun tab-clique--hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a session hotkey."
  (let ((entries (mapcar #'cl-third (tab-clique--hotkey-candidates))))
    (if entries
        (tab-clique--format-menu title entries)
      (concat
       (propertize (concat title "\n") 'face 'minibuffer-prompt)
       "No hotkeys assigned"))))

(defun tab-clique--bound-hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a bound hotkey."
  (tab-clique--format-menu
   title
   (mapcar #'cl-third (tab-clique--bound-hotkey-candidates))))

(defun tab-clique-read-hotkey (&optional title)
  "Prompt for a hotkey using `read-key' with TITLE."
  (read-key (tab-clique--hotkey-prompt (or title "Select hotkey"))))

(defun tab-clique-read-bound-hotkey (&optional title)
  "Prompt for a bound hotkey using `read-key' with TITLE."
  (let ((candidates (tab-clique--bound-hotkey-candidates)))
    (unless candidates
      (user-error "No session hotkeys are assigned"))
    (read-key (tab-clique--bound-hotkey-prompt
               (or title "Jump to session:")))))

(defun tab-clique--read-available-hotkey (name)
  "Prompt until an available hotkey is chosen for NAME."
  (let ((key (tab-clique-read-hotkey
              (format "Assign hotkey to %s" name))))
    (if-let* ((existing (tab-clique--hotkey-session key)))
        (if (equal existing name)
            key
          (message "Hotkey [%s] is already assigned to %s. Choose another."
                   (single-key-description key)
                   existing)
          (sit-for 1)
          (tab-clique--read-available-hotkey name))
      key)))

(defun tab-clique--rename-session-tabs (old-name new-name)
  "Rename OLD-NAME session tabs to NEW-NAME."
  (let ((current-tab (tab-bar--current-tab)))
    (dolist (tab (tab-clique--tabs-in-session old-name))
      (tab-clique--select-tab tab)
      (tab-bar-change-tab-group new-name))
    (tab-clique--select-tab current-tab)))

(defun tab-clique--session-selector-candidates ()
  "Return session candidates with single-key selectors.

Each item has the form (KEY NAME LABEL). KEY is the character
accepted by `read-key', NAME is the session name, and LABEL is the
string shown in the prompt."
  (let ((sessions (tab-clique--sorted-sessions))
        candidates)
    (when (> (length sessions) (length tab-clique--selector-key-preference))
      (user-error "Too many sessions for single-key selection"))
    (cl-loop
     for name in sessions
     for key in tab-clique--selector-key-preference
     do (push (list key
                    name
                    (concat
                     (propertize (format "[%c]" key)
                                 'face 'tab-clique-quick-select-key)
                     " "
                     (propertize name
                                 'face 'tab-clique-quick-select-session)))
              candidates))
    (nreverse candidates)))

(defun tab-clique--quick-select-prompt ()
  "Return a formatted prompt for quick session selection."
  (tab-clique--format-menu
   "Select session:"
   (mapcar #'cl-third (tab-clique--session-selector-candidates))))

(defun tab-clique--quick-select-session ()
  "Prompt for a session using a single key press."
  (let* ((candidates (tab-clique--session-selector-candidates))
         (prompt (tab-clique--quick-select-prompt))
         (key (read-key prompt))
         (selection (seq-find (lambda (candidate)
                                (= (car candidate) key))
                              candidates)))
    (unless selection
      (user-error "No session is bound to %s" (single-key-description key)))
    (cadr selection)))

(defun tab-clique--handle-tab-open (tab)
  "Initialize TAB after opening."
  (unless (alist-get 'group tab)
    (setf (alist-get 'group tab) tab-clique-default-session)))

(defun tab-clique--cleanup-removed-sessions (before-sessions)
  "Clear state for sessions in BEFORE-SESSIONS that no longer exist."
  (dolist (name before-sessions)
    (unless (member name (tab-clique--sessions))
      (tab-clique--clear-session-hotkey name)
      (when (equal tab-clique--last-session name)
        (setq tab-clique--last-session nil)))))

(defun tab-clique--advice-close-tab (orig &rest args)
  "Keep session state consistent around `tab-bar-close-tab' ORIG with ARGS."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (apply orig args)
    (let* ((before-sessions (tab-clique--sessions))
           (current-session (tab-clique--current))
           (last-tab-in-session-p (= (length (tab-clique--tabs-in-current-session)) 1))
           (fallback-session (and last-tab-in-session-p
                                  (tab-clique--fallback-session current-session))))
      (prog1
          ;; `tab-bar-close-tab' may re-select a surviving tab by its global
          ;; tab-bar index while tearing down the closed tab. Bypass
          ;; session-scoped advice for that internal selection.
          (apply #'tab-clique--call-unscoped orig args)
        (tab-clique--cleanup-removed-sessions before-sessions)
        (when (and fallback-session
                   (not (member current-session (tab-clique--sessions)))
                   (member fallback-session (tab-clique--sessions)))
          (tab-clique-switch fallback-session))))))

(defun tab-clique--advice-close-other-tabs (orig &optional tab-number)
  "Restrict `tab-bar-close-other-tabs' ORIG to the current session."
  (if (or tab-clique--inhibit-command-scoping
          (not tab-clique-mode))
      (funcall orig tab-number)
    (let* ((tabs (tab-clique--tabs-in-current-session))
           (target-tab (if tab-number
                           (nth (1- tab-number) tabs)
                         (tab-clique--current-tab-in-list (tab-clique--all-tabs)))))
      (unless target-tab
        (user-error "No such tab in current session: %s" tab-number))
      (tab-clique--select-tab target-tab)
      (dolist (tab (delq target-tab (copy-sequence tabs)))
        (when-let* ((position (seq-position (tab-clique--all-tabs) tab #'eq)))
          (tab-clique--call-unscoped #'tab-bar-close-tab (1+ position)))))))

;;; Sparse keymap (unbound, ready for future use)

(defvar tab-clique-keymap
  (let ((map (make-sparse-keymap)))
    ;; Example commands, ready to bind later if desired
    (define-key map (kbd "s") #'tab-clique-switch)
    (define-key map (kbd "S") #'tab-clique-switch-completing)
    (define-key map (kbd "l") #'tab-clique-switch-last)
    (define-key map (kbd "a") #'tab-clique-assign-hotkey)
    (define-key map (kbd "r") #'tab-clique-rename)
    (define-key map (kbd "j") #'tab-clique-jump-hotkey)
    (define-key map (kbd "n") #'tab-clique-new)
    (define-key map (kbd "k") #'tab-clique-kill)
    map)
  "Sparse keymap for `tab-clique-mode` commands.
Currently not bound to any prefix, ready for future keybindings.")

;;; Completing read candidates

(defun tab-clique--session-annotation (candidate)
  "Return completion annotation for session CANDIDATE."
  (if-let* ((key (tab-clique--session-hotkey candidate)))
      (format " [%s]" (single-key-description key))
    ""))

(defun tab-clique-read (&optional prompt)
  "Prompt for a session using `completing-read` with PROMPT."
  (let ((completion-extra-properties
         '(:category tab-clique-session
           :annotation-function tab-clique--session-annotation)))
    (completing-read (or prompt "Session: ")
                     (tab-clique--sessions)
                     nil t)))

(with-eval-after-load 'marginalia
  (defun marginalia-annotate-tab-clique-session (candidate)
    "Annotate tab-clique CANDIDATE with its assigned hotkey."
    (tab-clique--session-annotation candidate))
  (add-to-list 'marginalia-annotators
               '(tab-clique-session
                 marginalia-annotate-tab-clique-session
                 builtin
                 none)))

(defun tab-clique--read-switch-session ()
  "Read a session name for `tab-clique-switch'."
  (let ((sessions (tab-clique--sessions)))
    (cond
     ((null sessions)
      (user-error "No sessions available"))
     ((= (length sessions) 1)
      (car sessions))
     (t
      (tab-clique--quick-select-session)))))

;;; Commands

(defun tab-clique-switch-by-name (name)
  "Switch to session NAME.

This is the public entry point for switching sessions directly by
name."
  (interactive (list (tab-clique-read)))
  (unless (member name (tab-clique--sessions))
    (user-error "No such session: %s" name))
  (unless (equal name (tab-clique--current))
    (let ((tab (tab-clique--preferred-tab name)))
      (when tab
        (tab-clique--select-tab tab)))))

(defun tab-clique-switch (name)
  "Switch to session NAME."
  (interactive (list (tab-clique--read-switch-session)))
  (tab-clique-switch-by-name name))

(defun tab-clique-switch-completing (name)
  "Switch to session NAME using minibuffer completion."
  (interactive (list (tab-clique-read)))
  (tab-clique-switch name))

(defun tab-clique-switch-last ()
  "Switch to the previously active session."
  (interactive)
  (unless tab-clique--last-session
    (user-error "No previous session"))
  (unless (member tab-clique--last-session (tab-clique--sessions))
    (user-error "Previous session no longer exists"))
  (tab-clique-switch tab-clique--last-session))

(defun tab-clique-assign-hotkey (name key)
  "Assign hotkey KEY to session NAME."
  (interactive
   (let ((name (tab-clique-read)))
     (list name
           (tab-clique--read-available-hotkey name))))
  (setq key (tab-clique--assign-hotkey key name))
  (message "Bound session %s to [%s]" name (single-key-description key)))

(defun tab-clique-jump-hotkey (key)
  "Jump to the session bound to hotkey KEY."
  (interactive (list (tab-clique-read-bound-hotkey "Jump to session:")))
  (if-let* ((session (tab-clique--hotkey-session key)))
      (tab-clique-switch session)
    (message "No session is bound to [%s]" (single-key-description key))))

(defun tab-clique-new (name)
  "Create new session NAME."
  (interactive (list (read-string "New session: ")))
  (tab-bar-new-tab)
  (tab-clique--set name))

(defun tab-clique-rename (old-name new-name)
  "Rename session OLD-NAME to NEW-NAME."
  (interactive
   (let ((old-name (tab-clique-read)))
     (list old-name
           (read-string (format "Rename session %s to: " old-name)
                        old-name))))
  (when (string-empty-p new-name)
    (user-error "Session name cannot be empty"))
  (unless (member old-name (tab-clique--sessions))
    (user-error "No such session: %s" old-name))
  (unless (or (equal old-name new-name)
              (not (member new-name (tab-clique--sessions))))
    (user-error "Session already exists: %s" new-name))
  (unless (equal old-name new-name)
    (tab-clique--rename-session-tabs old-name new-name)
    (tab-clique--rename-hotkey-session old-name new-name)
    (tab-clique--rename-last-session old-name new-name)))

(defun tab-clique-kill (name)
  "Kill session NAME."
  (interactive (list (tab-clique-read)))
  (when (= (length (tab-clique--sessions)) 1)
    (user-error "Cannot kill the last session"))
  (tab-clique--clear-session-hotkey name)
  (when (equal tab-clique--last-session name)
    (setq tab-clique--last-session nil))
  (dolist (tab (tab-clique--tabs-in-session name))
    (tab-bar-close-tab (1+ (seq-position (tab-clique--all-tabs) tab #'eq)))))

;;; Minor mode

(define-minor-mode tab-clique-mode
  "Manage sessions using tab-bar groups, with mode line display."
  :global t
  ;; :keymap tab-clique-keymap
  (if tab-clique-mode
      (progn
        (setq tab-clique--saved-tab-bar-mode tab-bar-mode)
        (tab-bar-mode 1)
        (setq tab-clique--saved-tab-bar-tab-group-function tab-bar-tab-group-function)
        (setq tab-clique--saved-tab-bar-format tab-bar-format)
        (setq tab-clique--saved-tab-bar-show-inactive-group-tabs
              tab-bar-show-inactive-group-tabs)
        (setq tab-clique--saved-tab-bar-close-button-show
              tab-bar-close-button-show)
        (setq tab-clique--saved-tab-bar-auto-width
              tab-bar-auto-width)
        (setq tab-bar-tab-group-function #'tab-clique--tab-group)
        (setq tab-bar-format
              (tab-clique--tab-bar-format tab-bar-format))
        (setq tab-bar-show-inactive-group-tabs nil)
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-auto-width tab-clique-tab-bar-auto-width)
        (advice-add 'tab-next :around
                    #'tab-clique--advice-switch-to-next-tab)
        (advice-add 'tab-bar-switch-to-next-tab :around
                    #'tab-clique--advice-switch-to-next-tab)
        (advice-add 'tab-previous :around
                    #'tab-clique--advice-switch-to-prev-tab)
        (advice-add 'tab-bar-switch-to-prev-tab :around
                    #'tab-clique--advice-switch-to-prev-tab)
        (advice-add 'tab-bar-select-tab :around
                    #'tab-clique--advice-select-tab)
        (advice-add 'tab-bar-close-tab :around
                    #'tab-clique--advice-close-tab)
        (advice-add 'tab-bar-close-other-tabs :around
                    #'tab-clique--advice-close-other-tabs)
        (advice-add 'tab-move :around
                    #'tab-clique--advice-move-tab)
        (advice-add 'tab-bar-move-tab :around
                    #'tab-clique--advice-move-tab)
        (advice-add 'tab-bar-move-tab-to :around
                    #'tab-clique--advice-move-tab-to)
        (add-hook 'tab-bar-tab-post-open-functions
                  #'tab-clique--handle-tab-open)
        ;; Ensure startup tab has a session
        (tab-clique--ensure-current-session))
    ;; Disable
    (advice-remove 'tab-next
                   #'tab-clique--advice-switch-to-next-tab)
    (advice-remove 'tab-bar-switch-to-next-tab
                   #'tab-clique--advice-switch-to-next-tab)
    (advice-remove 'tab-previous
                   #'tab-clique--advice-switch-to-prev-tab)
    (advice-remove 'tab-bar-switch-to-prev-tab
                   #'tab-clique--advice-switch-to-prev-tab)
    (advice-remove 'tab-bar-select-tab
                   #'tab-clique--advice-select-tab)
    (advice-remove 'tab-bar-close-tab
                   #'tab-clique--advice-close-tab)
    (advice-remove 'tab-bar-close-other-tabs
                   #'tab-clique--advice-close-other-tabs)
    (advice-remove 'tab-move
                   #'tab-clique--advice-move-tab)
    (advice-remove 'tab-bar-move-tab
                   #'tab-clique--advice-move-tab)
    (advice-remove 'tab-bar-move-tab-to
                   #'tab-clique--advice-move-tab-to)
    (remove-hook 'tab-bar-tab-post-open-functions
                 #'tab-clique--handle-tab-open)
    (setq tab-bar-tab-group-function tab-clique--saved-tab-bar-tab-group-function)
    (setq tab-bar-format tab-clique--saved-tab-bar-format)
    (setq tab-bar-show-inactive-group-tabs
          tab-clique--saved-tab-bar-show-inactive-group-tabs)
    (setq tab-bar-close-button-show
          tab-clique--saved-tab-bar-close-button-show)
    (setq tab-bar-auto-width
          tab-clique--saved-tab-bar-auto-width)
    (when (null tab-clique--saved-tab-bar-mode)
      (tab-bar-mode 0))
    (setq tab-clique--saved-tab-bar-mode nil)
    (setq tab-clique--saved-tab-bar-tab-group-function nil)
    (setq tab-clique--saved-tab-bar-format nil)
    (setq tab-clique--saved-tab-bar-show-inactive-group-tabs nil)
    (setq tab-clique--saved-tab-bar-close-button-show nil)
    (setq tab-clique--saved-tab-bar-auto-width nil)))

;;; Startup safety

(add-hook 'emacs-startup-hook
          (lambda ()
            (when tab-clique-mode
              (tab-clique--ensure-current-session))))

(provide 'tab-clique)
;;; tab-clique.el ends here
