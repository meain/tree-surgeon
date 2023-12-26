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
config) to builtin treesit module. As of now, we just have split-join
and go-errors but should have more here soon.*

### Split join

Split and join arguments and parameters in function calls and definitions.

![](https://user-images.githubusercontent.com/14259816/258302386-d479cd0d-aa3e-4a2f-8f89-2959041a08c8.webm)

``` emacs-lisp
M-x tree-surgeon-split-join
```

### Go errors

Automatically generate go return statements for errors. Call
`tree-surgeon-go-errors` to generate the return statements with an `if
err != nil` check.

``` emacs-lisp
M-x tree-surgeon-go-errors
```

##### Customization

You can add or modify the language specific settings by modifying `tree-surgeon-split-join-settings`.

