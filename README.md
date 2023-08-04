# tree-surgeon

A collection of code manipulation utils using tree-sitter.

``` emacs-lisp
;; Example configuration
(use-package tree-surgeon
  :straight (tree-surgeon :type git :host github :repo "meain/tree-surgeon")
  :config (evil-leader/set-key "H j" 'tree-surgeon-split-join))
```

## Features

*I am still porting over things from elisp-tree-sitter(in my personal
config) to builtin treesit module. As of now, we just have split-join,
but should have more here soon.*

### Split join

Split and join arguments and parameters in function calls and definitions.


##### Usage

``` emacs-lisp
(tree-surgeon-split-join)
```

##### Customization

You can add or modify the language specific settings by modifying `tree-surgeon-split-join-settings`.

