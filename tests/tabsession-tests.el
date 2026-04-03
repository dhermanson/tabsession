;;; tabsession-tests.el --- ERT tests for tabsession -*- lexical-binding: t; -*-

(require 'ert)
(require 'tab-bar)
(require 'dired)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'tabsession)

(defun tabsession-test--reset-state ()
  "Reset tab-bar and tabsession state for tests."
  (when tabsession-mode
    (tabsession-mode 0))
  (tab-bar-mode 1)
  (while (> (length (tab-bar-tabs)) 1)
    (tab-bar-close-tab 2))
  (tab-bar-select-tab 1)
  (tab-bar-change-tab-group "")
  (setq tab-bar-format '(tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-add-tab))
  (setq tab-bar-show-inactive-group-tabs t)
  (setq tab-bar-tab-group-function #'tab-bar-tab-group-default)
  (setq global-mode-string nil)
  (setq tabsession-session-hotkeys nil)
  (setq tabsession--last-session nil))

(defmacro tabsession-test--with-reset (&rest body)
  "Run BODY after resetting tab-bar and tabsession state."
  `(unwind-protect
       (progn
         (tabsession-test--reset-state)
         ,@body)
     (tabsession-test--reset-state)))

(defun tabsession-test--interleaved-tabs ()
  "Create an interleaved tab layout for session-scoping tests."
  (tabsession-mode 1)
  (tab-bar-rename-tab "main-1")
  (tabsession-new "work")
  (tab-bar-rename-tab "work-1")
  (tabsession-switch "main")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "main-2")
  (tabsession-switch "main")
  (tabsession--select-tab (car (tabsession--tabs-in-session "main"))))

(ert-deftest tabsession-test-new-session-preserves-existing-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (should (equal (sort (tabsession--sessions) #'string<)
                  '("main" "work")))
   (should (equal (tabsession--current) "work"))))

(ert-deftest tabsession-test-switch-can-reach-hidden-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (should (equal (tabsession--current) "main"))
   (should (equal (sort (tabsession--sessions) #'string<)
                  '("main" "work")))))

(ert-deftest tabsession-test-switch-prefers-most-recent-tab-in-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (tabsession--set "work")
   (tab-bar-rename-tab "work-2")
   (tabsession-switch "main")
   (tabsession-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-2"))))

(ert-deftest tabsession-test-switch-falls-back-when-mru-tab-was-closed ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (tabsession--set "work")
   (tab-bar-rename-tab "work-2")
   (tab-bar-close-tab)
   (tabsession-switch "main")
   (tabsession-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-1"))))

(ert-deftest tabsession-test-read-switch-session-uses-first-selector-key ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?s)))
     (should (equal (tabsession--read-switch-session) "work")))))

(ert-deftest tabsession-test-read-switch-session-uses-selector-order ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "mail")
   (tabsession-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?a)))
     (should (equal (tabsession--read-switch-session) "mail")))))

(ert-deftest tabsession-test-quick-select-prompt-includes-highlighted-keys ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
     (tabsession-new "work")
     (tabsession-switch "main")
     (let ((prompt (tabsession--quick-select-prompt)))
     (should (string-match-p "Select session" prompt))
     (should (string-match-p "\\[a\\].*main" prompt))
     (should (string-match-p "\\[s\\].*work" prompt))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'tabsession-quick-select-key prompt)))))

(ert-deftest tabsession-test-session-selector-candidates-are-sorted ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "zeta")
   (tabsession-new "alpha")
   (tabsession-new "beta")
   (should (equal (mapcar #'cadr (tabsession--session-selector-candidates))
                  '("alpha" "beta" "main" "zeta")))))

(ert-deftest tabsession-test-session-selector-uses-fixed-qwerty-order ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "mail")
   (tabsession-switch "main")
   (should (equal (tabsession--session-selector-candidates)
                  '((97 "mail" "[a] mail")
                    (115 "main" "[s] main"))))))

(ert-deftest tabsession-test-quick-select-prompt-uses-six-row-columns ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (dolist (name '("alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf"))
     (tabsession-new name))
   (tabsession-switch "main")
   (let* ((prompt (tabsession--quick-select-prompt))
          (lines (split-string prompt "\n")))
     (should (= (length lines) 7))
     (should (string-match-p "\\[a\\].*alpha.*\\[l\\].*golf" (nth 1 lines)))
     (should (string-match-p "\\[s\\].*bravo.*\\[;\\].*main" (nth 2 lines)))
     (should (string-match-p "\\[k\\].*foxtrot" (nth 6 lines))))))

(ert-deftest tabsession-test-assign-hotkey-binds-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (should (equal (tabsession--hotkey-session ?a) "work"))
   (should (equal (tabsession--session-hotkey "work") ?a))))

(ert-deftest tabsession-test-session-completion-annotates-hotkeys ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (should (equal (tabsession--session-annotation "work") " [a]"))
   (should (equal (tabsession--session-annotation "main") ""))
   (cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt collection _predicate _require-match &rest _)
                (should (equal collection '("main" "work")))
                (should (equal (plist-get completion-extra-properties :category)
                               'tabsession-session))
                (should (eq (plist-get completion-extra-properties :annotation-function)
                            'tabsession--session-annotation))
                "work")))
     (should (equal (tabsession-read) "work")))))

(ert-deftest tabsession-test-hotkey-prompt-uses-grid-and-shows-assignments ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-new "alpha")
   (tabsession-assign-hotkey "work" ?z)
   (tabsession-assign-hotkey "alpha" ?a)
   (let* ((prompt (tabsession--hotkey-prompt "Assign hotkey to work"))
          (lines (split-string prompt "\n")))
     (should (string-match-p "Assign hotkey to work" prompt))
     (should (string-match-p "\\[a\\].*alpha" (nth 1 lines)))
     (should (string-match-p "\\[z\\].*work" (nth 2 lines)))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'tabsession-quick-select-key prompt)))))

(ert-deftest tabsession-test-bound-hotkey-prompt-shows-only-assigned-keys ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (let ((prompt (tabsession--bound-hotkey-prompt "Jump to session:")))
     (should (string-match-p "\\[a\\].*work" prompt))
     (should-not (string-match-p "unbound" prompt)))))

(ert-deftest tabsession-test-jump-hotkey-switches-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (tabsession-assign-hotkey "work" ?a)
   (tabsession-jump-hotkey ?a)
   (should (equal (tabsession--current) "work"))))

(ert-deftest tabsession-test-jump-hotkey-messages-when-unbound ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (should (equal (tabsession-jump-hotkey ?a)
                  "No session is bound to [a]"))))

(ert-deftest tabsession-test-read-available-hotkey-retries-on-conflict ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-new "mail")
   (tabsession-assign-hotkey "work" ?a)
   (let ((messages nil))
     (cl-letf (((symbol-function 'read-key)
                (let ((keys '(?a ?b)))
                  (lambda (_prompt)
                    (prog1 (car keys)
                      (setq keys (cdr keys))))))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages)))
               ((symbol-function 'sit-for)
                (lambda (&rest _) nil)))
       (should (equal (tabsession--read-available-hotkey "mail") ?b))
       (should (equal (car messages)
                      "Hotkey [a] is already assigned to work. Choose another."))))))

(ert-deftest tabsession-test-assign-hotkey-errors-when-key-already-used ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-new "mail")
   (tabsession-assign-hotkey "work" ?a)
   (should-error (tabsession-assign-hotkey "mail" ?a)
                 :type 'user-error)))

(ert-deftest tabsession-test-read-bound-hotkey-errors-when-none-assigned ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (should-error (tabsession-read-bound-hotkey)
                 :type 'user-error)))

(ert-deftest tabsession-test-switch-last-jumps-to-previous-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (tabsession-switch "work")
   (tabsession-switch-last)
   (should (equal (tabsession--current) "main"))
   (should (equal tabsession--last-session "work"))))

(ert-deftest tabsession-test-switch-last-toggles-between-two-sessions ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (tabsession-switch "work")
   (tabsession-switch-last)
   (tabsession-switch-last)
   (should (equal (tabsession--current) "work"))
   (should (equal tabsession--last-session "main"))))

(ert-deftest tabsession-test-switch-last-errors-without-previous-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (should-error (tabsession-switch-last)
                 :type 'user-error)))

(ert-deftest tabsession-test-switch-last-survives-rename ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (tabsession-rename "work" "deep-work")
   (tabsession-switch "deep-work")
   (tabsession-switch-last)
   (should (equal (tabsession--current) "main"))))

(ert-deftest tabsession-test-kill-clears-hotkey-binding ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (tabsession-kill "work")
   (should-not (tabsession--hotkey-session ?a))
   (should-not (tabsession--session-hotkey "work"))))

(ert-deftest tabsession-test-kill-clears-last-session-when-needed ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (tabsession-switch "work")
   (tabsession-kill "main")
   (should-not tabsession--last-session)
   (should-error (tabsession-switch-last)
                 :type 'user-error)))

(ert-deftest tabsession-test-rename-session-preserves-hotkey ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (tabsession-rename "work" "deep-work")
   (should (equal (tabsession--sessions) '("main" "deep-work")))
   (should (equal (tabsession--hotkey-session ?a) "deep-work"))
   (should-not (tabsession--session-hotkey "work"))
   (tabsession-jump-hotkey ?a)
   (should (equal (tabsession--current) "deep-work"))))

(ert-deftest tabsession-test-switch-completing-uses-completion ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (cl-letf (((symbol-function 'tabsession-read)
              (lambda () "work")))
     (call-interactively #'tabsession-switch-completing)
     (should (equal (tabsession--current) "work")))))

(ert-deftest tabsession-test-kill-removes-only-target-session ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-kill "main")
   (should (equal (tabsession--sessions) '("work")))
   (should (equal (tabsession--current) "work"))))

(ert-deftest tabsession-test-mode-restores-tab-bar-settings ()
  (tabsession-test--with-reset
   (let ((original-format '(tab-bar-format-menu-bar
                            tab-bar-format-tabs
                            tab-bar-separator))
         (original-group-function #'tab-bar-tab-group-default)
         (original-show-inactive t)
         (original-close-button-show t)
         (original-auto-width t))
     (setq tab-bar-format original-format)
     (setq tab-bar-tab-group-function original-group-function)
     (setq tab-bar-show-inactive-group-tabs original-show-inactive)
     (setq tab-bar-close-button-show original-close-button-show)
     (setq tab-bar-auto-width original-auto-width)
     (tabsession-mode 1)
     (should-not tab-bar-auto-width)
     (tabsession-mode 0)
     (should (equal tab-bar-format original-format))
     (should (eq tab-bar-tab-group-function original-group-function))
     (should (eq tab-bar-show-inactive-group-tabs original-show-inactive))
     (should (eq tab-bar-close-button-show original-close-button-show))
     (should (eq tab-bar-auto-width original-auto-width)))))

(ert-deftest tabsession-test-mode-hides-tab-close-button ()
  (tabsession-test--with-reset
   (setq tab-bar-close-button-show t)
   (tabsession-mode 1)
   (should-not tab-bar-close-button-show)
   (tabsession-mode 0)
   (should tab-bar-close-button-show)))

(ert-deftest tabsession-test-mode-enables-tab-bar-mode ()
  (tabsession-test--with-reset
   (tab-bar-mode 0)
   (tabsession-mode 1)
   (should tab-bar-mode)
   (tabsession-mode 0)
   (should-not tab-bar-mode)))

(ert-deftest tabsession-test-tab-next-stays-in-current-session ()
  (tabsession-test--with-reset
   (tabsession-test--interleaved-tabs)
   (tab-next)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest tabsession-test-tab-select-uses-current-session-order ()
  (tabsession-test--with-reset
   (tabsession-test--interleaved-tabs)
   (tab-bar-select-tab 2)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest tabsession-test-tab-close-other-stays-in-current-session ()
  (tabsession-test--with-reset
   (tabsession-test--interleaved-tabs)
   (tab-close-other)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "main"))
                  '("main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "work"))
                  '("work-1")))
   (should (equal (tabsession--sessions) '("main" "work")))))

(ert-deftest tabsession-test-tab-close-does-not-use-global-index-in-session-scope ()
  (tabsession-test--with-reset
   (tabsession-test--interleaved-tabs)
   (should (eq (condition-case nil
                   (progn
                     (tab-bar-close-tab 1)
                     'ok)
                 (user-error 'user-error))
               'ok))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "main"))
                  '("main-2")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest tabsession-test-tab-move-stays-in-current-session ()
  (tabsession-test--with-reset
   (tabsession-test--interleaved-tabs)
   (tabsession--select-tab (car (last (tabsession--tabs-in-session "main"))))
   (tab-move -1)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "main"))
                  '("main-2" "main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tabsession--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest tabsession-test-tab-bar-hides-inactive-session-entries ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (let ((labels
          (mapcar #'caddr (tabsession--format-tabs))))
     (should (member "main" labels))
     (should (seq-some (lambda (label)
                         (string-match-p "\\*scratch\\*" label))
                       labels))
     (should-not (member "work" labels)))))

(ert-deftest tabsession-test-mode-survives-dired-buffer-switch ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (should (equal (tabsession--current) "main"))
   (dired default-directory)
   (should (equal (tabsession--current) "main"))))
