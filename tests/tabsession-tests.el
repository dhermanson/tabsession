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
  (setq tabsession-show-inactive-groups-in-tab-bar t)
  (setq tabsession-session-hotkeys nil)
  (setq tabsession--last-session nil))

(defmacro tabsession-test--with-reset (&rest body)
  "Run BODY after resetting tab-bar and tabsession state."
  `(unwind-protect
       (progn
         (tabsession-test--reset-state)
         ,@body)
     (tabsession-test--reset-state)))

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

(ert-deftest tabsession-test-hotkey-prompt-uses-grid-and-shows-assignments ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (let* ((prompt (tabsession--hotkey-prompt "Assign hotkey to work"))
          (lines (split-string prompt "\n")))
     (should (string-match-p "Assign hotkey to work" prompt))
     (should (string-match-p "\\[a\\].*work" (nth 1 lines)))
     (should (string-match-p "\\[s\\].*unbound" (nth 2 lines)))
     (should (eq (get-text-property 0 'face prompt)
                 'minibuffer-prompt))
     (should (text-property-any
              0 (length prompt) 'face 'tabsession-quick-select-key prompt)))))

(ert-deftest tabsession-test-bound-hotkey-prompt-shows-only-assigned-keys ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-assign-hotkey "work" ?a)
   (let ((prompt (tabsession--bound-hotkey-prompt "Jump to session hotkey")))
     (should (string-match-p "\\[a\\].*work" prompt))
     (should-not (string-match-p "\\[s\\].*unbound" prompt)))))

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
         (original-show-inactive t))
     (setq tab-bar-format original-format)
     (setq tab-bar-tab-group-function original-group-function)
     (setq tab-bar-show-inactive-group-tabs original-show-inactive)
     (tabsession-mode 1)
     (tabsession-mode 0)
     (should (equal tab-bar-format original-format))
     (should (eq tab-bar-tab-group-function original-group-function))
     (should (eq tab-bar-show-inactive-group-tabs original-show-inactive)))))

(ert-deftest tabsession-test-tab-bar-hides-inactive-session-entries ()
  (tabsession-test--with-reset
   (setq tabsession-show-inactive-groups-in-tab-bar nil)
   (tabsession-mode 1)
   (tabsession-new "work")
   (tabsession-switch "main")
   (let ((labels
          (mapcar #'caddr (tabsession--format-tabs))))
     (should (member "main" labels))
     (should (member "*scratch* x" labels))
     (should-not (member "work" labels)))))

(ert-deftest tabsession-test-tab-bar-shows-all-session-groups-when-enabled ()
  (tabsession-test--with-reset
   (setq tabsession-show-inactive-groups-in-tab-bar t)
   (tabsession-mode 1)
   (tabsession-new "work")
   (tab-bar-rename-tab "work-1")
   (tabsession-switch "main")
   (let ((labels
          (mapcar #'caddr (tab-bar-format-tabs-groups))))
     (should (member "main" labels))
     (should (member "*scratch* x" labels))
     (should (member "work" labels))
     (should-not (member "work-1 x" labels)))))

(ert-deftest tabsession-test-mode-survives-dired-buffer-switch ()
  (tabsession-test--with-reset
   (tabsession-mode 1)
   (should (equal (tabsession--current) "main"))
   (dired default-directory)
   (should (equal (tabsession--current) "main"))))
