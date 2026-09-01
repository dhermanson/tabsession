;;; pivot-treemacs.el --- Treemacs scopes for Pivot sessions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Use a Pivot session, rather than a tab's mutable display name, as the scope
;; for a Treemacs buffer and workspace.

;;; Code:

(require 'pivot)
(require 'treemacs)
(require 'treemacs-tab-bar)

(declare-function pivot-current "pivot" (&optional frame))
(declare-function treemacs--on-scope-kill "treemacs-scope" (scope))
(declare-function treemacs-tab-bar--on-tab-switch "treemacs-tab-bar" (&rest args))
(declare-function treemacs-do-rename-workspace "treemacs-workspaces"
                  (&optional workspace new-name))

(defclass treemacs-pivot-scope (treemacs-scope) () :abstract t)

(add-to-list 'treemacs-scope-types (cons 'Pivot 'treemacs-pivot-scope))

(defun pivot-treemacs--scope (session &optional frame)
  "Return the Treemacs scope for SESSION on FRAME."
  (cons (or frame (selected-frame)) session))

(defun pivot-treemacs--workspace-name (session)
  "Return the Treemacs workspace name for SESSION."
  (format "Pivot %s" session))

(cl-defmethod treemacs-scope->current-scope ((_ (subclass treemacs-pivot-scope)))
  "Return the current frame-local Pivot session scope."
  (let ((frame (selected-frame)))
    (pivot-treemacs--scope (pivot-current frame) frame)))

(cl-defmethod treemacs-scope->current-scope-name
  ((_ (subclass treemacs-pivot-scope)) scope)
  "Return a display name for the Pivot SCOPE."
  (pivot-treemacs--workspace-name (cdr scope)))

(cl-defmethod treemacs-scope->setup ((_ (subclass treemacs-pivot-scope)))
  "Set up Pivot-backed Treemacs scopes."
  (add-hook 'pivot-session-switch-functions
            #'pivot-treemacs--on-session-switch)
  (add-hook 'pivot-session-renamed-functions
            #'pivot-treemacs--on-session-renamed)
  (add-hook 'pivot-session-killed-functions
            #'pivot-treemacs--on-session-killed)
  (treemacs-tab-bar--ensure-workspace-exists))

(cl-defmethod treemacs-scope->cleanup ((_ (subclass treemacs-pivot-scope)))
  "Tear down Pivot-backed Treemacs scopes."
  (remove-hook 'pivot-session-switch-functions
               #'pivot-treemacs--on-session-switch)
  (remove-hook 'pivot-session-renamed-functions
               #'pivot-treemacs--on-session-renamed)
  (remove-hook 'pivot-session-killed-functions
               #'pivot-treemacs--on-session-killed))

(defun pivot-treemacs--on-session-switch (_from _to frame)
  "Select the local Treemacs workspace after a session switch on FRAME."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (treemacs-tab-bar--on-tab-switch))))

(defun pivot-treemacs--on-session-renamed (old-name new-name frame)
  "Migrate Treemacs state after renaming a session on FRAME."
  (let* ((old-scope (pivot-treemacs--scope old-name frame))
         (new-scope (pivot-treemacs--scope new-name frame))
         (mapping (assoc old-scope treemacs--scope-storage)))
    (when mapping
      (setcar mapping new-scope)
      (let* ((shelf (cdr mapping))
             (workspace (treemacs-scope-shelf->workspace shelf))
             (old-workspace-name (pivot-treemacs--workspace-name old-name)))
        (when (and workspace
                   (equal (treemacs-workspace->name workspace)
                          old-workspace-name))
          (treemacs-do-rename-workspace
           workspace
           (pivot-treemacs--workspace-name new-name)))))))

(defun pivot-treemacs--on-session-killed (name frame)
  "Remove the Treemacs scope for session NAME on FRAME."
  (treemacs--on-scope-kill (pivot-treemacs--scope name frame)))

(provide 'pivot-treemacs)
;;; pivot-treemacs.el ends here
