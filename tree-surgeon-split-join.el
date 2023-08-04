;;; tree-surgeon-split-join.el --- Split or join arguments -*- lexical-binding: t; -*-

;; Keywords: tree-sitter, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;;; Commentary:
;; Move to meain/tree-surgeon.el.  It will collect all the little
;; tree-sitter based thingies once we have a few.

;; Version with elisp-tree-sitter support:
;; https://github.com/meain/dotfiles/blob/98db8bdefe475d8b9e27640d87ffa5869595a5c8/emacs/.config/emacs/tree-surgeon-split-join.el

;;; Code:

(require 'treesit)

(defun tree-surgeon-split-join--closest-node (nodes)
  "Find node closest to point from `NODES'."
  (let ((distance 999999)
        (node nil)
        (point (point)))
    (progn
      (seq-do (lambda (x)
                (let* (
                       (start (treesit-node-start (cdr x)))
                       (end (treesit-node-end (cdr x)))
                       (smallest (min (abs (- start point))
                                      (abs (- end point)))))
                  (unless (or (< distance smallest)
                              (> start point)
                              (< end point))
                    (setq node x)
                    (setq distance smallest))))
              nodes)
      node)))

(defcustom tree-surgeon-split-join-settings
  '( ; (mode . ((node_types) . comma))
    (json-ts-mode . ((object) . none))
    (rust-ts-mode . ((parameters arguments) . trailing))
    (python-ts-mode . ((parameters argument_list) . trailing))
    (ruby-ts-mode  . ((method_parameters argument_list) . none))
    ;; (nix-mode .((formals) . leading))
    ;; (js-mode . ((formal_parameters arguments) . trailing))
    (go-ts-mode . ((argument_list parameter_list literal_value) . trailing)))
  "Mapping from major modes to node types and comma placement."
  :type '(alist
          :key-type symbol
          :value-type (cons
                       (repeat symbol)
                       (choice (const none)
                               (const trailing)
                               (const leading))))
  :group 'tree-surgeon)

;;;###autoload
(defun tree-surgeon-split-join ()
  "Split or join arguments."
  (interactive)
  (when-let* ((settings (alist-get major-mode tree-surgeon-split-join-settings))
              (root (treesit-buffer-root-node))
              (node-selectors (car settings))
              (string-query-builder (lambda (x) (concat "(" (symbol-name x) ") @list")))
              (string-query (string-join (mapcar string-query-builder node-selectors) " "))

              (nodes (treesit-query-capture root string-query))
              (node (cdr (tree-surgeon-split-join--closest-node nodes)))
              (children (treesit-node-children node t))

              (start (treesit-node-start node))
              (end (treesit-node-end node))

              (text-pieces (seq-map (lambda (x) (treesit-node-text x)) children))

              (joined (string-join text-pieces ", "))
              (comma (cdr settings))
              (split (cond
                      ((string-match-p "\n" (treesit-node-text node)) joined)
                      ((equal comma 'none) (concat "\n" (string-join text-pieces ",\n") "\n"))
                      ((equal comma 'trailing) (concat "\n" (string-join text-pieces ",\n") ",\n"))
                      ((equal comma 'leading) (concat " " (string-join text-pieces "\n, ") "\n")))))

    (unless (= (length children) 0)
      (goto-char (+ start 1))
      (delete-char (- end start 2))
      (insert split)
      (indent-region start (+ 1 (point)))
      (goto-char start))))

(provide 'tree-surgeon-split-join)
;;; tree-surgeon-split-join.el ends here