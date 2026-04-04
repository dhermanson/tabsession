# tab-clique.el

`tab-clique.el` provides tmux-like sessions on top of Emacs `tab-bar`.

Each tab belongs to a tab-bar group, and `tab-clique-mode` treats that group
as the tab's session. When the mode is enabled:

- each new tab inherits the current session
- switching sessions selects a tab from that session
- killing a session closes all tabs in that session
- the tab bar only shows tabs from the current session

This gives you separate tab collections that behave more like independent
workspaces, while still using the built-in `tab-bar` implementation underneath.

## Commands

- `M-x tab-clique-mode` enables or disables the global minor mode
- `M-x tab-clique-new` creates a new session in a new tab
- `M-x tab-clique-switch` switches to an existing session, using a single-key
  picker by default when multiple sessions exist
- `M-x tab-clique-switch-completing` switches to an existing session using
  minibuffer completion
- `M-x tab-clique-switch-last` jumps back to the previously active session
- `M-x tab-clique-assign-hotkey` assigns a session to a jump hotkey
- `M-x tab-clique-jump-hotkey` jumps directly to the session on a hotkey
- `M-x tab-clique-rename` renames an existing session
- `M-x tab-clique-kill` closes all tabs in a session

By default, tabs without an explicit group are treated as part of the `"main"`
session.

## Configuration

If you use `use-package`, configure it like this:

```elisp
(use-package tab-clique
  :vc (:url "https://github.com/dhermanson/tab-clique" :rev :newest)
  :bind-keymap
  ("C-c t g" . tab-clique-keymap)
  ("s-t" . tab-clique-keymap)
  :config
  (tab-clique-mode 1))
```

`tab-clique-mode` shows the current session label in the tab bar while keeping
inactive session headers and tabs hidden.

By default, `tab-clique-mode` disables Emacs' `tab-bar-auto-width`, so each tab
can shrink to its label width instead of stretching across the tab bar. Set
`tab-clique-tab-bar-auto-width` to non-nil if you prefer the built-in resizing.

Use `tab-clique-tab-label-padding` and `tab-clique-tab-group-label-padding` to
control the horizontal buffer around tab names and the current session label.

`tab-clique-jump-hotkey` uses the same styled `read-key` menu as session
switching and shows any assigned hotkeys directly in the prompt.

If you use Marginalia, session completion also shows any assigned hotkey as an
annotation. Marginalia is optional and not required for `tab-clique`.

## Testing

Run the ERT test suite in batch mode from the repository root:

```sh
emacs -Q --batch -L . -L tests -l tests/tab-clique-tests.el -f ert-run-tests-batch-and-exit
```
