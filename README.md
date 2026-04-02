# tabsession.el

`tabsession.el` provides tmux-like sessions on top of Emacs `tab-bar`.

Each tab belongs to a tab-bar group, and `tabsession-mode` treats that group
as the tab's session. When the mode is enabled:

- each new tab inherits the current session
- switching sessions selects a tab from that session
- killing a session closes all tabs in that session
- the tab bar only shows tabs from the current session

This gives you separate tab collections that behave more like independent
workspaces, while still using the built-in `tab-bar` implementation underneath.

## Commands

- `M-x tabsession-mode` enables or disables the global minor mode
- `M-x tabsession-new` creates a new session in a new tab
- `M-x tabsession-switch` switches to an existing session, using a single-key
  picker by default when multiple sessions exist
- `M-x tabsession-switch-completing` switches to an existing session using
  minibuffer completion
- `M-x tabsession-switch-last` jumps back to the previously active session
- `M-x tabsession-assign-hotkey` assigns a session to a jump hotkey
- `M-x tabsession-jump-hotkey` jumps directly to the session on a hotkey
- `M-x tabsession-rename` renames an existing session
- `M-x tabsession-kill` closes all tabs in a session

By default, tabs without an explicit group are treated as part of the `"main"`
session.

## Configuration

If you use `use-package`, configure it like this:

```elisp
(use-package tabsession
  :bind-keymap
  ("C-c t g" . tabsession-keymap)
  ("s-t" . tabsession-keymap)
  :config
  (tabsession-mode 1))
```

`tabsession-mode` shows the current session label in the tab bar while keeping
inactive session headers and tabs hidden.

`tabsession-jump-hotkey` uses the same styled `read-key` menu as session
switching and shows any assigned hotkeys directly in the prompt.

## Testing

Run the ERT test suite in batch mode from the repository root:

```sh
emacs -Q --batch -L . -L tests -l tests/tabsession-tests.el -f ert-run-tests-batch-and-exit
```
