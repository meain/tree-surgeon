;;; tree-surgeon.el --- A collection of code manipulation utils using tree-sitter -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/tree-surgeon
;; Keywords: tree-sitter, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;;; Commentary:
;; This packages provides a collection of utils to manipulate code using tree-sitter.

;;; Code:

(require 'treesit)
(require 'tree-surgeon-split-join)

(defgroup tree-surgeon nil
  "A collection of code manipulation utils using tree-sitter."
  :group 'tools)


(provide 'tree-surgeon)
;;; tree-surgeon.el ends here