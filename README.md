# tabsession.el

`tabsession.el` helps you pivot between activities on top of Emacs `tab-bar`.

Each tab belongs to a tab-bar group, and `tabsession-mode` treats that group
as an activity. An activity is a named set of tabs for one thread of work:
a feature, a bug, a customer issue, research, or whatever you want to keep
separate from the rest of your editor.

When the mode is enabled:

- each new tab inherits the current activity
- switching activities selects a tab from that activity
- killing an activity closes all tabs in that activity
- the tab bar only shows tabs from the current activity

This gives you lightweight workspaces without replacing Emacs' built-in tab
system. You stay in `tab-bar`, but the package makes it practical to keep
distinct contexts isolated and jump between them quickly.

## Why use it

`tabsession.el` is useful when plain tabs are too flat, but a full workspace or
perspective system feels heavier than you want. It gives you:

- focused tab sets for each activity instead of one long global tab list
- fast context switches between tasks
- a simple mental model built directly on top of `tab-bar`
- minimal friction, since it uses Emacs' existing tab implementation

## Commands

- `M-x tabsession-mode` enables or disables the global minor mode
- `M-x tabsession-new` creates a new activity in a new tab
- `M-x tabsession-switch` switches to an existing activity, using a single-key
  picker by default when multiple activities exist
- `M-x tabsession-switch-completing` switches to an existing activity using
  minibuffer completion
- `M-x tabsession-switch-last` jumps back to the previously active activity
- `M-x tabsession-assign-hotkey` assigns an activity to a jump hotkey
- `M-x tabsession-jump-hotkey` jumps directly to the activity on a hotkey
- `M-x tabsession-rename` renames an existing activity
- `M-x tabsession-kill` closes all tabs in an activity

By default, tabs without an explicit group are treated as part of the `"main"`
activity.

## Configuration

If you use `use-package`, configure it like this:

```elisp
(use-package tabsession
  :vc (:url "https://github.com/dhermanson/tabsession" :rev :newest)
  :bind-keymap
  ("C-c t g" . tabsession-keymap)
  ("s-t" . tabsession-keymap)
  :config
  (tabsession-mode 1))
```

`tabsession-mode` shows the current activity label in the tab bar while keeping
inactive activity headers and tabs hidden.

By default, `tabsession-mode` disables Emacs' `tab-bar-auto-width`, so each tab
can shrink to its label width instead of stretching across the tab bar. Set
`tabsession-tab-bar-auto-width` to non-nil if you prefer the built-in resizing.

Use `tabsession-tab-label-padding` and `tabsession-tab-group-label-padding` to
control the horizontal buffer around tab names and the current activity label.

`tabsession-jump-hotkey` uses the same styled `read-key` menu as activity
switching and shows any assigned hotkeys directly in the prompt.

If you use Marginalia, activity completion also shows any assigned hotkey as an
annotation. Marginalia is optional and not required for `tabsession`.

## Testing

Run the ERT test suite in batch mode from the repository root:

```sh
emacs -Q --batch -L . -L tests -l tests/tabsession-tests.el -f ert-run-tests-batch-and-exit
```

## Ideas

- [ ] add the ability to move a tab to a different session
