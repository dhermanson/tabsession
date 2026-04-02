;;; tabsession.el --- tmux-like sessions using tab-bar groups -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tab-bar)
(require 'seq)

;;; Customization

(defgroup tabsession nil
  "Session management using tab-bar groups."
  :group 'convenience)

(defface tabsession-quick-select-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for keys in the quick session selector."
  :group 'tabsession)

(defface tabsession-quick-select-session
  '((t :inherit default))
  "Face used for session names in the quick session selector."
  :group 'tabsession)

(defcustom tabsession-default-session "main"
  "Default session name."
  :type 'string)

(defcustom tabsession-show-groups-in-tab-bar nil
  "Whether to show session group labels in the tab bar.

When non-nil, `tabsession-mode' replaces `tab-bar-format-tabs'
with `tab-bar-format-tabs-groups'.  When nil, tabs stay grouped
internally for session behavior, but the tab bar renders plain tabs
without the extra group labels."
  :type 'boolean)

;;; Mode line

(defvar tabsession--mode-line-string nil
  "String displayed in the mode line for the current tab session.")

(defconst tabsession--mode-line-segment
  '(:eval tabsession--mode-line-string)
  "Mode line segment used by `tabsession-mode'.")

(defvar tabsession--saved-tab-bar-tab-group-function nil
  "Original value of `tab-bar-tab-group-function'
before enabling `tabsession-mode'.")

(defvar tabsession--saved-tab-bar-format nil
  "Original value of `tab-bar-format' before enabling `tabsession-mode'.")

(defvar tabsession--saved-tab-bar-show-inactive-group-tabs nil
  "Original value of `tab-bar-show-inactive-group-tabs' before enabling mode.")

(defconst tabsession--selector-key-preference
  (string-to-list "asdfjkl;ghwertyuiopcvbnmzqx1234567890")
  "Preferred key order for quick session selection on a QWERTY layout.")

(defvar tabsession-session-hotkeys nil
  "Alist mapping hotkey characters to session names.")

(defvar tabsession--last-session nil
  "Previously active session name.")

;;; Core helpers

(defun tabsession--tab-group (tab)
  "Return the session group for TAB."
  (or (alist-get 'group tab)
      tabsession-default-session))

(defun tabsession--current ()
  "Return the current session name."
  (tabsession--tab-group (tab-bar--current-tab)))

(defun tabsession--record-session-transition (from to)
  "Record a session transition from FROM to TO."
  (when (and from to (not (equal from to)))
    (setq tabsession--last-session from)))

(defun tabsession--all-tabs (&optional frame)
  "Return the complete tab list for FRAME."
  (tab-bar-tabs frame))

(defun tabsession--set (name)
  "Assign current tab to session NAME."
  (tab-bar-change-tab-group name)
  (tabsession--update-mode-line))

(defun tabsession--tabs (&optional frame)
  "Return all tabs for FRAME."
  (tabsession--all-tabs frame))

(defun tabsession--ensure-current-session ()
  "Assign the current tab to the default session if it has no group."
  (unless (alist-get 'group (tab-bar--current-tab))
    (tabsession--set tabsession-default-session)))

(defun tabsession--select-tab (tab)
  "Select TAB from the complete tab list."
  (let* ((current-session (tabsession--current))
         (tabs (tabsession--all-tabs))
         (index (seq-position tabs tab #'eq)))
    (when index
      (tabsession--record-session-transition
       current-session
       (tabsession--tab-group tab))
      (tab-bar-select-tab (1+ index)))))

(defun tabsession--tab-bar-format (format)
  "Return FORMAT adjusted for `tabsession-mode'."
  (mapcar (lambda (item)
            (if (memq item '(tab-bar-format-tabs tab-bar-format-tabs-groups))
                'tabsession--format-tabs
              item))
          format))

(defun tabsession--format-tabs ()
  "Produce tab-bar items for the current session only."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-group (funcall tab-bar-tab-group-function
                                 (tab-bar--current-tab-find tabs)))
         (visible-tabs
          (seq-filter
           (lambda (tab)
             (equal (funcall tab-bar-tab-group-function tab) current-group))
           tabs))
         (i 0))
    (append
     (when (and tabsession-show-groups-in-tab-bar current-group visible-tabs)
       (tab-bar--format-tab-group (car visible-tabs) 1 t))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (let ((tab-bar-tab-face-function tab-bar-tab-group-face-function))
          (tab-bar--format-tab tab i)))
      visible-tabs))))

(defun tabsession--sessions ()
  "Return all session names."
  (delete-dups
   (mapcar #'tabsession--tab-group
           (tabsession--all-tabs))))

(defun tabsession--tabs-in-session (name)
  (seq-filter
   (lambda (tab)
     (equal (tabsession--tab-group tab) name))
   (tabsession--all-tabs)))

(defun tabsession--preferred-tab (name)
  "Return the preferred tab to select for session NAME."
  (car (seq-sort-by
        (lambda (tab)
          (or (alist-get 'time tab) 0))
        #'>
        (tabsession--tabs-in-session name))))

(defun tabsession--sorted-sessions ()
  "Return session names sorted alphabetically."
  (sort (copy-sequence (tabsession--sessions)) #'string-lessp))

(defun tabsession--format-menu (title entries)
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

(defun tabsession--normalize-hotkey (key)
  "Return normalized hotkey character for KEY."
  (let ((normalized (downcase key)))
    (unless (memq normalized tabsession--selector-key-preference)
      (user-error "Hotkey must be one of: %s"
                  (string-join
                   (mapcar #'char-to-string
                           tabsession--selector-key-preference)
                   " ")))
    normalized))

(defun tabsession--session-hotkey (name)
  "Return the hotkey assigned to session NAME, or nil."
  (car (rassoc name tabsession-session-hotkeys)))

(defun tabsession--hotkey-session (key)
  "Return the session assigned to hotkey KEY, or nil."
  (alist-get key tabsession-session-hotkeys))

(defun tabsession--clear-session-hotkey (name)
  "Remove any hotkey bound to session NAME."
  (setq tabsession-session-hotkeys
        (cl-remove name tabsession-session-hotkeys
                   :key #'cdr
                   :test #'equal)))

(defun tabsession--assign-hotkey (key name)
  "Assign hotkey KEY to session NAME."
  (setq key (tabsession--normalize-hotkey key))
  (tabsession--clear-session-hotkey name)
  (setf (alist-get key tabsession-session-hotkeys) name)
  key)

(defun tabsession--rename-hotkey-session (old-name new-name)
  "Update hotkey bindings from OLD-NAME to NEW-NAME."
  (let ((key (tabsession--session-hotkey old-name)))
    (when key
      (setf (alist-get key tabsession-session-hotkeys) new-name))))

(defun tabsession--rename-last-session (old-name new-name)
  "Update last-session tracking from OLD-NAME to NEW-NAME."
  (when (equal tabsession--last-session old-name)
    (setq tabsession--last-session new-name)))

(defun tabsession--hotkey-candidates ()
  "Return hotkey candidates in selector order.

Each item has the form (KEY SESSION LABEL)."
  (mapcar
   (lambda (key)
     (let ((session (tabsession--hotkey-session key)))
       (list key
             session
             (concat
              (propertize (format "[%c]" key)
                          'face 'tabsession-quick-select-key)
              " "
              (propertize (or session "unbound")
                          'face 'tabsession-quick-select-session)))))
   tabsession--selector-key-preference))

(defun tabsession--bound-hotkey-candidates ()
  "Return bound hotkey candidates in selector order.

Each item has the form (KEY SESSION LABEL)."
  (seq-filter
   (lambda (candidate)
     (cadr candidate))
   (tabsession--hotkey-candidates)))

(defun tabsession--hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a session hotkey."
  (tabsession--format-menu
   title
   (mapcar #'cl-third (tabsession--hotkey-candidates))))

(defun tabsession--bound-hotkey-prompt (title)
  "Return a prompt with TITLE for selecting a bound hotkey."
  (tabsession--format-menu
   title
   (mapcar #'cl-third (tabsession--bound-hotkey-candidates))))

(defun tabsession-read-hotkey (&optional title)
  "Prompt for a hotkey using `read-key' with TITLE."
  (tabsession--normalize-hotkey
   (read-key (tabsession--hotkey-prompt (or title "Select hotkey")))))

(defun tabsession-read-bound-hotkey (&optional title)
  "Prompt for a bound hotkey using `read-key' with TITLE."
  (let ((candidates (tabsession--bound-hotkey-candidates)))
    (unless candidates
      (user-error "No session hotkeys are assigned"))
    (tabsession--normalize-hotkey
     (read-key (tabsession--bound-hotkey-prompt
                (or title "Jump to session hotkey"))))))

(defun tabsession--rename-session-tabs (old-name new-name)
  "Rename OLD-NAME session tabs to NEW-NAME."
  (let ((current-tab (tab-bar--current-tab)))
    (dolist (tab (tabsession--tabs-in-session old-name))
      (tabsession--select-tab tab)
      (tab-bar-change-tab-group new-name))
    (tabsession--select-tab current-tab)))

(defun tabsession--session-selector-candidates ()
  "Return session candidates with single-key selectors.

Each item has the form (KEY NAME LABEL). KEY is the character
accepted by `read-key', NAME is the session name, and LABEL is the
string shown in the prompt."
  (let ((sessions (tabsession--sorted-sessions))
        candidates)
    (when (> (length sessions) (length tabsession--selector-key-preference))
      (user-error "Too many sessions for single-key selection"))
    (cl-loop
     for name in sessions
     for key in tabsession--selector-key-preference
     do (push (list key
                    name
                    (concat
                     (propertize (format "[%c]" key)
                                 'face 'tabsession-quick-select-key)
                     " "
                     (propertize name
                                 'face 'tabsession-quick-select-session)))
              candidates))
    (nreverse candidates)))

(defun tabsession--quick-select-prompt ()
  "Return a formatted prompt for quick session selection."
  (tabsession--format-menu
   "Select session"
   (mapcar #'cl-third (tabsession--session-selector-candidates))))

(defun tabsession--quick-select-session ()
  "Prompt for a session using a single key press."
  (let* ((candidates (tabsession--session-selector-candidates))
         (prompt (tabsession--quick-select-prompt))
         (key (read-key prompt))
         (selection (seq-find (lambda (candidate)
                                (= (car candidate) key))
                              candidates)))
    (unless selection
      (user-error "No session is bound to %s" (single-key-description key)))
    (cadr selection)))

;;; Mode line

(defun tabsession--update-mode-line (&rest _)
  "Update mode line with current session."
  (setq tabsession--mode-line-string
        (format " [%s] " (tabsession--current)))
  (force-mode-line-update t))

(defun tabsession--handle-tab-open (tab)
  "Initialize TAB after opening and refresh the mode line."
  (unless (alist-get 'group tab)
    (setf (alist-get 'group tab) tabsession-default-session))
  (tabsession--update-mode-line))

(defun tabsession--handle-tab-select (_previous-tab _tab)
  "Refresh the mode line after selecting a tab."
  (tabsession--update-mode-line))

(defun tabsession--handle-tab-change-group (_tab)
  "Refresh the mode line after changing a tab group."
  (tabsession--update-mode-line))

;;; Sparse keymap (unbound, ready for future use)

(defvar tabsession-keymap
  (let ((map (make-sparse-keymap)))
    ;; Example commands, ready to bind later if desired
    (define-key map (kbd "s") #'tabsession-switch)
    (define-key map (kbd "S") #'tabsession-switch-completing)
    (define-key map (kbd "l") #'tabsession-switch-last)
    (define-key map (kbd "a") #'tabsession-assign-hotkey)
    (define-key map (kbd "r") #'tabsession-rename)
    (define-key map (kbd "j") #'tabsession-jump-hotkey)
    (define-key map (kbd "n") #'tabsession-new)
    (define-key map (kbd "k") #'tabsession-kill)
    map)
  "Sparse keymap for `tabsession-mode` commands.
Currently not bound to any prefix, ready for future keybindings.")

;;; Completing read candidates

(defun tabsession-read ()
  "Prompt for a session using `completing-read`."
  (completing-read "Session: " (tabsession--sessions) nil t))

(defun tabsession--read-switch-session ()
  "Read a session name for `tabsession-switch'."
  (let ((sessions (tabsession--sessions)))
    (cond
     ((null sessions)
      (user-error "No sessions available"))
     ((= (length sessions) 1)
      (car sessions))
     (t
      (tabsession--quick-select-session)))))

;;; Commands

(defun tabsession-switch (name)
  "Switch to session NAME."
  (interactive (list (tabsession--read-switch-session)))
  (let ((tab (tabsession--preferred-tab name)))
    (when tab
      (tabsession--select-tab tab))))

(defun tabsession-switch-completing (name)
  "Switch to session NAME using minibuffer completion."
  (interactive (list (tabsession-read)))
  (tabsession-switch name))

(defun tabsession-switch-last ()
  "Switch to the previously active session."
  (interactive)
  (unless tabsession--last-session
    (user-error "No previous session"))
  (unless (member tabsession--last-session (tabsession--sessions))
    (user-error "Previous session no longer exists"))
  (tabsession-switch tabsession--last-session))

(defun tabsession-assign-hotkey (name key)
  "Assign hotkey KEY to session NAME."
  (interactive
   (let ((name (tabsession-read)))
     (list name
           (tabsession-read-hotkey
            (format "Assign hotkey to %s" name)))))
  (setq key (tabsession--assign-hotkey key name))
  (message "Bound session %s to [%c]" name key))

(defun tabsession-jump-hotkey (key)
  "Jump to the session bound to hotkey KEY."
  (interactive (list (tabsession-read-bound-hotkey "Jump to session hotkey")))
  (setq key (tabsession--normalize-hotkey key))
  (if-let* ((session (tabsession--hotkey-session key)))
      (tabsession-switch session)
    (message "No session is bound to [%c]" key)))

(defun tabsession-new (name)
  "Create new session NAME."
  (interactive "sNew session: ")
  (tab-bar-new-tab)
  (tabsession--set name))

(defun tabsession-rename (old-name new-name)
  "Rename session OLD-NAME to NEW-NAME."
  (interactive
   (let ((old-name (tabsession-read)))
     (list old-name
           (read-string (format "Rename session %s to: " old-name)
                        old-name))))
  (when (string-empty-p new-name)
    (user-error "Session name cannot be empty"))
  (unless (member old-name (tabsession--sessions))
    (user-error "No such session: %s" old-name))
  (unless (or (equal old-name new-name)
              (not (member new-name (tabsession--sessions))))
    (user-error "Session already exists: %s" new-name))
  (unless (equal old-name new-name)
    (tabsession--rename-session-tabs old-name new-name)
    (tabsession--rename-hotkey-session old-name new-name)
    (tabsession--rename-last-session old-name new-name)
    (tabsession--update-mode-line)))

(defun tabsession-kill (name)
  "Kill session NAME."
  (interactive (list (tabsession-read)))
  (when (= (length (tabsession--sessions)) 1)
    (user-error "Cannot kill the last session"))
  (tabsession--clear-session-hotkey name)
  (when (equal tabsession--last-session name)
    (setq tabsession--last-session nil))
  (dolist (tab (tabsession--tabs-in-session name))
    (tab-bar-close-tab (1+ (seq-position (tabsession--all-tabs) tab #'eq)))))

;;; Minor mode

(define-minor-mode tabsession-mode
  "Manage sessions using tab-bar groups, with mode line display."
  :global t
  ;; :keymap tabsession-keymap
  (if tabsession-mode
      (progn
        (setq tabsession--saved-tab-bar-tab-group-function tab-bar-tab-group-function)
        (setq tabsession--saved-tab-bar-format tab-bar-format)
        (setq tabsession--saved-tab-bar-show-inactive-group-tabs
              tab-bar-show-inactive-group-tabs)
        ;; Assign tab group and visibility logic
        (setq tab-bar-tab-group-function #'tabsession--tab-group)
        (setq tab-bar-format
              (tabsession--tab-bar-format tab-bar-format))
        (setq tab-bar-show-inactive-group-tabs nil)
        (unless (member tabsession--mode-line-segment global-mode-string)
          (setq global-mode-string
                (append global-mode-string
                        (list tabsession--mode-line-segment))))
        (add-hook 'tab-bar-tab-post-open-functions
                  #'tabsession--handle-tab-open)
        (add-hook 'tab-bar-tab-post-select-functions
                  #'tabsession--handle-tab-select)
        (add-hook 'tab-bar-tab-post-change-group-functions
                  #'tabsession--handle-tab-change-group)
        ;; Ensure startup tab has a session
        (tabsession--ensure-current-session)
        (tabsession--update-mode-line))
    ;; Disable
    (remove-hook 'tab-bar-tab-post-open-functions
                 #'tabsession--handle-tab-open)
    (remove-hook 'tab-bar-tab-post-select-functions
                 #'tabsession--handle-tab-select)
    (remove-hook 'tab-bar-tab-post-change-group-functions
                 #'tabsession--handle-tab-change-group)
    (setq tab-bar-tab-group-function tabsession--saved-tab-bar-tab-group-function)
    (setq tab-bar-format tabsession--saved-tab-bar-format)
    (setq tab-bar-show-inactive-group-tabs
          tabsession--saved-tab-bar-show-inactive-group-tabs)
    (setq tabsession--saved-tab-bar-tab-group-function nil)
    (setq tabsession--saved-tab-bar-format nil)
    (setq tabsession--saved-tab-bar-show-inactive-group-tabs nil)
    (setq global-mode-string
          (remove tabsession--mode-line-segment global-mode-string))
    (setq tabsession--mode-line-string nil)))

;;; Startup safety

(add-hook 'emacs-startup-hook
          (lambda ()
            (when tabsession-mode
              (tabsession--ensure-current-session)
              (tabsession--update-mode-line))))

(provide 'tabsession)
;;; tabsession.el ends here
