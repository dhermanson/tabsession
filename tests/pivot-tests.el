;;; pivot-tests.el --- ERT tests for pivot -*- lexical-binding: t; -*-

(require 'ert)
(require 'tab-bar)
(require 'dired)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'pivot)

(defun pivot-test--reset-state ()
  "Reset tab-bar and pivot state for tests."
  (when pivot-mode
    (pivot-mode 0))
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
  (setq pivot-tab-group-label-padding " ")
  (setq pivot-session-hotkeys nil)
  (setq pivot--last-session nil))

(defmacro pivot-test--with-reset (&rest body)
  "Run BODY after resetting tab-bar and pivot state."
  `(unwind-protect
       (progn
         (pivot-test--reset-state)
         ,@body)
     (pivot-test--reset-state)))

(defun pivot-test--interleaved-tabs ()
  "Create an interleaved tab layout for session-scoping tests."
  (pivot-mode 1)
  (tab-bar-rename-tab "main-1")
  (pivot-new "work")
  (tab-bar-rename-tab "work-1")
  (pivot-switch "main")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "main-2")
  (pivot-switch "main")
  (pivot--select-tab (car (pivot--tabs-in-session "main"))))

(ert-deftest pivot-test-new-session-preserves-existing-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (should (equal (sort (pivot--sessions) #'string<)
                  '("main" "work")))
   (should (equal (pivot--current) "work"))))

(ert-deftest pivot-test-switch-can-reach-hidden-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (should (equal (pivot--current) "main"))
   (should (equal (sort (pivot--sessions) #'string<)
                  '("main" "work")))))

(ert-deftest pivot-test-switch-prefers-most-recent-tab-in-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (pivot--set "work")
   (tab-bar-rename-tab "work-2")
   (pivot-switch "main")
   (pivot-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-2"))))

(ert-deftest pivot-test-switch-falls-back-when-mru-tab-was-closed ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (tab-bar-rename-tab "work-1")
   (tab-bar-new-tab)
   (pivot--set "work")
   (tab-bar-rename-tab "work-2")
   (tab-bar-close-tab)
   (pivot-switch "main")
   (pivot-switch "work")
   (should (equal (alist-get 'name (tab-bar--current-tab)) "work-1"))))

(ert-deftest pivot-test-read-switch-session-uses-first-selector-key ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?s)))
     (should (equal (pivot--read-switch-session) "work")))))

(ert-deftest pivot-test-read-switch-session-uses-selector-order ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "mail")
   (pivot-switch "main")
   (cl-letf (((symbol-function 'read-key)
              (lambda (_prompt) ?a)))
     (should (equal (pivot--read-switch-session) "mail")))))

(ert-deftest pivot-test-quick-select-prompt-includes-highlighted-keys ()
  (pivot-test--with-reset
   (pivot-mode 1)
     (pivot-new "work")
     (pivot-switch "main")
     (let ((prompt (pivot--quick-select-prompt)))
     (should (string-match-p "Select session" prompt))
     (should (string-match-p "\\[a\\].*main" prompt))
     (should (string-match-p "\\[s\\].*work" prompt))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'pivot-quick-select-key prompt)))))

(ert-deftest pivot-test-session-selector-candidates-are-sorted ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "zeta")
   (pivot-new "alpha")
   (pivot-new "beta")
   (should (equal (mapcar #'cadr (pivot--session-selector-candidates))
                  '("alpha" "beta" "main" "zeta")))))

(ert-deftest pivot-test-session-selector-uses-fixed-qwerty-order ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "mail")
   (pivot-switch "main")
   (should (equal (pivot--session-selector-candidates)
                  '((97 "mail" "[a] mail")
                    (115 "main" "[s] main"))))))

(ert-deftest pivot-test-quick-select-prompt-uses-six-row-columns ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (dolist (name '("alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf"))
     (pivot-new name))
   (pivot-switch "main")
   (let* ((prompt (pivot--quick-select-prompt))
          (lines (split-string prompt "\n")))
     (should (= (length lines) 7))
     (should (string-match-p "\\[a\\].*alpha.*\\[l\\].*golf" (nth 1 lines)))
     (should (string-match-p "\\[s\\].*bravo.*\\[;\\].*main" (nth 2 lines)))
     (should (string-match-p "\\[k\\].*foxtrot" (nth 6 lines))))))

(ert-deftest pivot-test-assign-hotkey-binds-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-assign-hotkey "work" ?a)
   (should (equal (pivot--hotkey-session ?a) "work"))
   (should (equal (pivot--session-hotkey "work") ?a))))

(ert-deftest pivot-test-session-completion-annotates-hotkeys ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-assign-hotkey "work" ?a)
   (should (equal (pivot--session-annotation "work") " [a]"))
   (should (equal (pivot--session-annotation "main") ""))
   (cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt collection _predicate _require-match &rest _)
                (should (equal collection '("main" "work")))
                (should (equal (plist-get completion-extra-properties :category)
                               'pivot-session))
                (should (eq (plist-get completion-extra-properties :annotation-function)
                            'pivot--session-annotation))
                "work")))
     (should (equal (pivot-read) "work")))))

(ert-deftest pivot-test-hotkey-prompt-uses-grid-and-shows-assignments ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-new "alpha")
   (pivot-assign-hotkey "work" ?z)
   (pivot-assign-hotkey "alpha" ?a)
   (let* ((prompt (pivot--hotkey-prompt "Assign hotkey to work"))
          (lines (split-string prompt "\n")))
     (should (string-match-p "Assign hotkey to work" prompt))
     (should (string-match-p "\\[a\\].*alpha" (nth 1 lines)))
     (should (string-match-p "\\[z\\].*work" (nth 2 lines)))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'pivot-quick-select-key prompt)))))

(ert-deftest pivot-test-bound-hotkey-prompt-shows-only-assigned-keys ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-assign-hotkey "work" ?a)
   (let ((prompt (pivot--bound-hotkey-prompt "Jump to session:")))
     (should (string-match-p "\\[a\\].*work" prompt))
     (should-not (string-match-p "unbound" prompt)))))

(ert-deftest pivot-test-jump-hotkey-switches-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (pivot-assign-hotkey "work" ?a)
   (pivot-jump-hotkey ?a)
   (should (equal (pivot--current) "work"))))

(ert-deftest pivot-test-jump-hotkey-messages-when-unbound ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (should (equal (pivot-jump-hotkey ?a)
                  "No session is bound to [a]"))))

(ert-deftest pivot-test-read-available-hotkey-retries-on-conflict ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-new "mail")
   (pivot-assign-hotkey "work" ?a)
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
       (should (equal (pivot--read-available-hotkey "mail") ?b))
       (should (equal (car messages)
                      "Hotkey [a] is already assigned to work. Choose another."))))))

(ert-deftest pivot-test-assign-hotkey-errors-when-key-already-used ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-new "mail")
   (pivot-assign-hotkey "work" ?a)
   (should-error (pivot-assign-hotkey "mail" ?a)
                 :type 'user-error)))

(ert-deftest pivot-test-read-bound-hotkey-errors-when-none-assigned ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (should-error (pivot-read-bound-hotkey)
                 :type 'user-error)))

(ert-deftest pivot-test-switch-last-jumps-to-previous-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (pivot-switch "work")
   (pivot-switch-last)
   (should (equal (pivot--current) "main"))
   (should (equal pivot--last-session "work"))))

(ert-deftest pivot-test-switch-last-toggles-between-two-sessions ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (pivot-switch "work")
   (pivot-switch-last)
   (pivot-switch-last)
   (should (equal (pivot--current) "work"))
   (should (equal pivot--last-session "main"))))

(ert-deftest pivot-test-switch-last-errors-without-previous-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (should-error (pivot-switch-last)
                 :type 'user-error)))

(ert-deftest pivot-test-switch-last-survives-rename ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (pivot-rename "work" "deep-work")
   (pivot-switch "deep-work")
   (pivot-switch-last)
   (should (equal (pivot--current) "main"))))

(ert-deftest pivot-test-kill-clears-hotkey-binding ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-assign-hotkey "work" ?a)
   (pivot-kill "work")
   (should-not (pivot--hotkey-session ?a))
   (should-not (pivot--session-hotkey "work"))))

(ert-deftest pivot-test-kill-clears-last-session-when-needed ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (pivot-switch "work")
   (pivot-kill "main")
   (should-not pivot--last-session)
   (should-error (pivot-switch-last)
                 :type 'user-error)))

(ert-deftest pivot-test-rename-session-preserves-hotkey ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-assign-hotkey "work" ?a)
   (pivot-rename "work" "deep-work")
   (should (equal (pivot--sessions) '("main" "deep-work")))
   (should (equal (pivot--hotkey-session ?a) "deep-work"))
   (should-not (pivot--session-hotkey "work"))
   (pivot-jump-hotkey ?a)
   (should (equal (pivot--current) "deep-work"))))

(ert-deftest pivot-test-switch-completing-uses-completion ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (cl-letf (((symbol-function 'pivot-read)
              (lambda () "work")))
     (call-interactively #'pivot-switch-completing)
     (should (equal (pivot--current) "work")))))

(ert-deftest pivot-test-kill-removes-only-target-session ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-kill "main")
   (should (equal (pivot--sessions) '("work")))
   (should (equal (pivot--current) "work"))))

(ert-deftest pivot-test-mode-restores-tab-bar-settings ()
  (pivot-test--with-reset
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
     (pivot-mode 1)
     (should-not tab-bar-auto-width)
     (pivot-mode 0)
     (should (equal tab-bar-format original-format))
     (should (eq tab-bar-tab-group-function original-group-function))
     (should (eq tab-bar-show-inactive-group-tabs original-show-inactive))
     (should (eq tab-bar-close-button-show original-close-button-show))
     (should (eq tab-bar-auto-width original-auto-width)))))

(ert-deftest pivot-test-mode-hides-tab-close-button ()
  (pivot-test--with-reset
   (setq tab-bar-close-button-show t)
   (pivot-mode 1)
   (should-not tab-bar-close-button-show)
   (pivot-mode 0)
   (should tab-bar-close-button-show)))

(ert-deftest pivot-test-mode-enables-tab-bar-mode ()
  (pivot-test--with-reset
   (tab-bar-mode 0)
   (pivot-mode 1)
   (should tab-bar-mode)
   (pivot-mode 0)
   (should-not tab-bar-mode)))

(ert-deftest pivot-test-tab-next-stays-in-current-session ()
  (pivot-test--with-reset
   (pivot-test--interleaved-tabs)
   (tab-next)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest pivot-test-tab-select-uses-current-session-order ()
  (pivot-test--with-reset
   (pivot-test--interleaved-tabs)
   (tab-bar-select-tab 2)
   (should (equal (alist-get 'name (tab-bar--current-tab)) "main-2"))))

(ert-deftest pivot-test-tab-close-other-stays-in-current-session ()
  (pivot-test--with-reset
   (pivot-test--interleaved-tabs)
   (tab-close-other)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "main"))
                  '("main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "work"))
                  '("work-1")))
   (should (equal (pivot--sessions) '("main" "work")))))

(ert-deftest pivot-test-tab-close-does-not-use-global-index-in-session-scope ()
  (pivot-test--with-reset
   (pivot-test--interleaved-tabs)
   (should (eq (condition-case nil
                   (progn
                     (tab-bar-close-tab 1)
                     'ok)
                 (user-error 'user-error))
               'ok))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "main"))
                  '("main-2")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest pivot-test-tab-move-stays-in-current-session ()
  (pivot-test--with-reset
   (pivot-test--interleaved-tabs)
   (pivot--select-tab (car (last (pivot--tabs-in-session "main"))))
   (tab-move -1)
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "main"))
                  '("main-2" "main-1")))
   (should (equal (mapcar (lambda (tab) (alist-get 'name tab))
                          (pivot--tabs-in-session "work"))
                  '("work-1")))))

(ert-deftest pivot-test-tab-bar-hides-inactive-session-entries ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (pivot-new "work")
   (pivot-switch "main")
   (let ((labels
          (mapcar #'caddr (pivot--format-tabs))))
     (should (seq-some (lambda (label)
                         (string-match-p "main" label))
                       labels))
     (should (seq-some (lambda (label)
                         (string-match-p "\\*scratch\\*" label))
                       labels))
     (should-not (member "work" labels)))))

(ert-deftest pivot-test-current-session-label-uses-padding ()
  (pivot-test--with-reset
   (setq pivot-tab-group-label-padding "  ")
   (pivot-mode 1)
   (let ((labels
          (mapcar #'caddr (pivot--format-tabs))))
     (should (member "  main  " labels)))))

(ert-deftest pivot-test-mode-survives-dired-buffer-switch ()
  (pivot-test--with-reset
   (pivot-mode 1)
   (should (equal (pivot--current) "main"))
   (dired default-directory)
   (should (equal (pivot--current) "main"))))
