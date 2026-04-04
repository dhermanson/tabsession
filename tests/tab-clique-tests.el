;;; tab-clique-tests.el --- ERT tests for tab-clique -*- lexical-binding: t; -*-

(require 'ert)
(require 'tab-bar)
(require 'dired)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'tab-clique)

(defun tab-clique-test--reset-state ()
  "Reset tab-bar and tab-clique state for tests."
  (when tab-clique-mode
    (tab-clique-mode 0))
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
  (setq tab-clique-tab-group-label-padding " ")
  (setq tab-clique-session-hotkeys nil)
  (setq tab-clique--last-session nil))

(defmacro tab-clique-test--with-reset (&rest body)
  "Run BODY after resetting tab-bar and tab-clique state."
  `(unwind-protect
       (progn
         (tab-clique-test--reset-state)
         ,@body)
     (tab-clique-test--reset-state)))

(defun tab-clique-test--interleaved-tabs ()
  "Create an interleaved tab layout for session-scoping tests."
  (tab-clique-mode 1)
  (tab-bar-rename-tab "main-1")
  (tab-clique-new "work")
  (tab-bar-rename-tab "work-1")
  (tab-clique-switch "main")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "main-2")
  (tab-clique-switch "main")
  (tab-clique--select-tab (car (tab-clique--tabs-in-session "main"))))

(ert-deftest tab-clique-test-new-session-preserves-existing-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (should (equal (sort (tab-clique--sessions) #'string<)
                  '("main" "work")))
   (should (equal (tab-clique--current) "work"))))

(ert-deftest tab-clique-test-switch-can-reach-hidden-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (should (equal (tab-clique--current) "main"))
   (should (equal (sort (tab-clique--sessions) #'string<)
                  '("main" "work")))))

(ert-deftest tab-clique-test-switch-prefers-most-recent-tab-in-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (tab-clique--set "work")
   (tab-bar-rename-tab "work-2")
   (tab-clique-switch "main")
   (tab-clique-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-2"))))

(ert-deftest tab-clique-test-switch-falls-back-when-mru-tab-was-closed ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (tab-clique--set "work")
   (tab-bar-rename-tab "work-2")
   (tab-bar-close-tab)
   (tab-clique-switch "main")
   (tab-clique-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-1"))))

(ert-deftest tab-clique-test-read-switch-session-uses-first-selector-key ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?s)))
     (should (equal (tab-clique--read-switch-session) "work")))))

(ert-deftest tab-clique-test-read-switch-session-uses-selector-order ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "mail")
   (tab-clique-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?a)))
     (should (equal (tab-clique--read-switch-session) "mail")))))

(ert-deftest tab-clique-test-quick-select-prompt-includes-highlighted-keys ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
     (tab-clique-new "work")
     (tab-clique-switch "main")
     (let ((prompt (tab-clique--quick-select-prompt)))
     (should (string-match-p "Select session" prompt))
     (should (string-match-p "\\[a\\].*main" prompt))
     (should (string-match-p "\\[s\\].*work" prompt))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'tab-clique-quick-select-key prompt)))))

(ert-deftest tab-clique-test-session-selector-candidates-are-sorted ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "zeta")
   (tab-clique-new "alpha")
   (tab-clique-new "beta")
   (should (equal (mapcar #'cadr (tab-clique--session-selector-candidates))
                  '("alpha" "beta" "main" "zeta")))))

(ert-deftest tab-clique-test-session-selector-uses-fixed-qwerty-order ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "mail")
   (tab-clique-switch "main")
   (should (equal (tab-clique--session-selector-candidates)
                  '((97 "mail" "[a] mail")
                    (115 "main" "[s] main"))))))

(ert-deftest tab-clique-test-quick-select-prompt-uses-six-row-columns ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (dolist (name '("alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf"))
     (tab-clique-new name))
   (tab-clique-switch "main")
   (let* ((prompt (tab-clique--quick-select-prompt))
          (lines (split-string prompt "\n")))
     (should (= (length lines) 7))
     (should (string-match-p "\\[a\\].*alpha.*\\[l\\].*golf" (nth 1 lines)))
     (should (string-match-p "\\[s\\].*bravo.*\\[;\\].*main" (nth 2 lines)))
     (should (string-match-p "\\[k\\].*foxtrot" (nth 6 lines))))))

(ert-deftest tab-clique-test-assign-hotkey-binds-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-assign-hotkey "work" ?a)
   (should (equal (tab-clique--hotkey-session ?a) "work"))
   (should (equal (tab-clique--session-hotkey "work") ?a))))

(ert-deftest tab-clique-test-session-completion-annotates-hotkeys ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-assign-hotkey "work" ?a)
   (should (equal (tab-clique--session-annotation "work") " [a]"))
   (should (equal (tab-clique--session-annotation "main") ""))
   (cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt collection _predicate _require-match &rest _)
                (should (equal collection '("main" "work")))
                (should (equal (plist-get completion-extra-properties :category)
                               'tab-clique-session))
                (should (eq (plist-get completion-extra-properties :annotation-function)
                            'tab-clique--session-annotation))
                "work")))
     (should (equal (tab-clique-read) "work")))))

(ert-deftest tab-clique-test-hotkey-prompt-uses-grid-and-shows-assignments ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-new "alpha")
   (tab-clique-assign-hotkey "work" ?z)
   (tab-clique-assign-hotkey "alpha" ?a)
   (let* ((prompt (tab-clique--hotkey-prompt "Assign hotkey to work"))
          (lines (split-string prompt "\n")))
     (should (string-match-p "Assign hotkey to work" prompt))
     (should (string-match-p "\\[a\\].*alpha" (nth 1 lines)))
     (should (string-match-p "\\[z\\].*work" (nth 2 lines)))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'tab-clique-quick-select-key prompt)))))

(ert-deftest tab-clique-test-bound-hotkey-prompt-shows-only-assigned-keys ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-assign-hotkey "work" ?a)
   (let ((prompt (tab-clique--bound-hotkey-prompt "Jump to session:")))
     (should (string-match-p "\\[a\\].*work" prompt))
     (should-not (string-match-p "unbound" prompt)))))

(ert-deftest tab-clique-test-jump-hotkey-switches-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (tab-clique-assign-hotkey "work" ?a)
   (tab-clique-jump-hotkey ?a)
   (should (equal (tab-clique--current) "work"))))

(ert-deftest tab-clique-test-jump-hotkey-messages-when-unbound ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (should (equal (tab-clique-jump-hotkey ?a)
                  "No session is bound to [a]"))))

(ert-deftest tab-clique-test-read-available-hotkey-retries-on-conflict ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-new "mail")
   (tab-clique-assign-hotkey "work" ?a)
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
       (should (equal (tab-clique--read-available-hotkey "mail") ?b))
       (should (equal (car messages)
                      "Hotkey [a] is already assigned to work. Choose another."))))))

(ert-deftest tab-clique-test-assign-hotkey-errors-when-key-already-used ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-new "mail")
   (tab-clique-assign-hotkey "work" ?a)
   (should-error (tab-clique-assign-hotkey "mail" ?a)
                 :type 'user-error)))

(ert-deftest tab-clique-test-read-bound-hotkey-errors-when-none-assigned ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (should-error (tab-clique-read-bound-hotkey)
                 :type 'user-error)))

(ert-deftest tab-clique-test-switch-last-jumps-to-previous-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (tab-clique-switch "work")
   (tab-clique-switch-last)
   (should (equal (tab-clique--current) "main"))
   (should (equal tab-clique--last-session "work"))))

(ert-deftest tab-clique-test-switch-last-toggles-between-two-sessions ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (tab-clique-switch "work")
   (tab-clique-switch-last)
   (tab-clique-switch-last)
   (should (equal (tab-clique--current) "work"))
   (should (equal tab-clique--last-session "main"))))

(ert-deftest tab-clique-test-switch-last-errors-without-previous-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (should-error (tab-clique-switch-last)
                 :type 'user-error)))

(ert-deftest tab-clique-test-switch-last-survives-rename ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (tab-clique-rename "work" "deep-work")
   (tab-clique-switch "deep-work")
   (tab-clique-switch-last)
   (should (equal (tab-clique--current) "main"))))

(ert-deftest tab-clique-test-kill-clears-hotkey-binding ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-assign-hotkey "work" ?a)
   (tab-clique-kill "work")
   (should-not (tab-clique--hotkey-session ?a))
   (should-not (tab-clique--session-hotkey "work"))))

(ert-deftest tab-clique-test-kill-clears-last-session-when-needed ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (tab-clique-switch "work")
   (tab-clique-kill "main")
   (should-not tab-clique--last-session)
   (should-error (tab-clique-switch-last)
                 :type 'user-error)))

(ert-deftest tab-clique-test-rename-session-preserves-hotkey ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-assign-hotkey "work" ?a)
   (tab-clique-rename "work" "deep-work")
   (should (equal (tab-clique--sessions) '("main" "deep-work")))
   (should (equal (tab-clique--hotkey-session ?a) "deep-work"))
   (should-not (tab-clique--session-hotkey "work"))
   (tab-clique-jump-hotkey ?a)
   (should (equal (tab-clique--current) "deep-work"))))

(ert-deftest tab-clique-test-switch-completing-uses-completion ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (cl-letf (((symbol-function 'tab-clique-read)
              (lambda () "work")))
     (call-interactively #'tab-clique-switch-completing)
     (should (equal (tab-clique--current) "work")))))

(ert-deftest tab-clique-test-kill-removes-only-target-session ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-kill "main")
   (should (equal (tab-clique--sessions) '("work")))
   (should (equal (tab-clique--current) "work"))))

(ert-deftest tab-clique-test-mode-restores-tab-bar-settings ()
  (tab-clique-test--with-reset
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
     (tab-clique-mode 1)
     (should-not tab-bar-auto-width)
     (tab-clique-mode 0)
     (should (equal tab-bar-format original-format))
     (should (eq tab-bar-tab-group-function original-group-function))
     (should (eq tab-bar-show-inactive-group-tabs original-show-inactive))
     (should (eq tab-bar-close-button-show original-close-button-show))
     (should (eq tab-bar-auto-width original-auto-width)))))

(ert-deftest tab-clique-test-mode-hides-tab-close-button ()
  (tab-clique-test--with-reset
   (setq tab-bar-close-button-show t)
   (tab-clique-mode 1)
   (should-not tab-bar-close-button-show)
   (tab-clique-mode 0)
   (should tab-bar-close-button-show)))

(ert-deftest tab-clique-test-mode-enables-tab-bar-mode ()
  (tab-clique-test--with-reset
   (tab-bar-mode 0)
   (tab-clique-mode 1)
   (should tab-bar-mode)
   (tab-clique-mode 0)
   (should-not tab-bar-mode)))

(ert-deftest tab-clique-test-tab-next-stays-in-current-session ()
  (tab-clique-test--with-reset
   (tab-clique-test--interleaved-tabs)
   (tab-next)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest tab-clique-test-tab-select-uses-current-session-order ()
  (tab-clique-test--with-reset
   (tab-clique-test--interleaved-tabs)
   (tab-bar-select-tab 2)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest tab-clique-test-tab-close-other-stays-in-current-session ()
  (tab-clique-test--with-reset
   (tab-clique-test--interleaved-tabs)
   (tab-close-other)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "main"))
                  '("main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "work"))
                  '("work-1")))
   (should (equal (tab-clique--sessions) '("main" "work")))))

(ert-deftest tab-clique-test-tab-close-does-not-use-global-index-in-session-scope ()
  (tab-clique-test--with-reset
   (tab-clique-test--interleaved-tabs)
   (should (eq (condition-case nil
                   (progn
                     (tab-bar-close-tab 1)
                     'ok)
                 (user-error 'user-error))
               'ok))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "main"))
                  '("main-2")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest tab-clique-test-tab-move-stays-in-current-session ()
  (tab-clique-test--with-reset
   (tab-clique-test--interleaved-tabs)
   (tab-clique--select-tab (car (last (tab-clique--tabs-in-session "main"))))
   (tab-move -1)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "main"))
                  '("main-2" "main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (tab-clique--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest tab-clique-test-tab-bar-hides-inactive-session-entries ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (tab-clique-new "work")
   (tab-clique-switch "main")
   (let ((labels
          (mapcar #'caddr (tab-clique--format-tabs))))
     (should (seq-some (lambda (label)
                         (string-match-p "main" label))
                       labels))
     (should (seq-some (lambda (label)
                         (string-match-p "\\*scratch\\*" label))
                       labels))
     (should-not (member "work" labels)))))

(ert-deftest tab-clique-test-current-session-label-uses-padding ()
  (tab-clique-test--with-reset
   (setq tab-clique-tab-group-label-padding "  ")
   (tab-clique-mode 1)
   (let ((labels
          (mapcar #'caddr (tab-clique--format-tabs))))
     (should (member "  main  " labels)))))

(ert-deftest tab-clique-test-mode-survives-dired-buffer-switch ()
  (tab-clique-test--with-reset
   (tab-clique-mode 1)
   (should (equal (tab-clique--current) "main"))
   (dired default-directory)
   (should (equal (tab-clique--current) "main"))))
