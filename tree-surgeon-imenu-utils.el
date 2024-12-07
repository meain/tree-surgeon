;;; tree-surgeon-imenu-utils.el -- Utilities for creating imenu interfaces -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utility functions for creating imenu interfaces using tree-sitter.
;; It includes functions for converting a list of items into a nested list and for generating imenu indexes.

;;; Code:
(require 'treesit)

(defun tree-surgeon--convert-to-nested-list (kvs)
  "Convert list of items into a nested list `KVS' based on their start and end.
The items are sorted by their start position and then processed in
order, with each item being added to the current nesting level if it
starts after the previous item ends."
  (let ((stack '())
        (result '()))
    (dolist (kv (sort kvs (lambda (a b) (< (nth 1 a) (nth 1 b)))) result)
      (while (and stack (> (nth 1 kv) (nth 2 (car stack))))
        (setq stack (cdr stack)))
      (let ((nested-name (append (mapcar #'car stack) (list (car kv)))))
        (push (list nested-name (nth 1 kv) (nth 2 kv)) result)
        (push kv stack)))
    (nreverse result)))

(defun tree-surgeon--kv-nesting-nodes (lang value-query key-query)
  "Find key value pairs for different levels.
The nodes are then used to generate a list of key-value pairs, where
the key is the text of the key node and the value is the start and end
positions of the node.  LANG is the language to use for the query,
VALUE-QUERY is the query to use to find the nodes, and KEY-QUERY is
the query to use to find the key within each node."
  (when-let* ((query (treesit-query-compile lang value-query))
              (captures (treesit-query-capture (treesit-buffer-root-node) query))
              (kvs (seq-map (lambda (c)
                              (let* ((node (cdr c))
                                     (key (cdr (car (treesit-query-capture node key-query)))))
                                (list (treesit-node-text key t) (treesit-node-start node) (treesit-node-end node))))
                            captures)))
    (tree-surgeon--convert-to-nested-list kvs)))

(defun tree-surgeon-kv-imenu-index-function (lang value-query key-query)
  "Return a function that generate an imenu index.
The function uses tree-surgeon--kv-nesting-nodes to generate a list of
key-value pairs, and then converts each pair into an imenu index item.
LANG is the language to use for the query, VALUE-QUERY is the query to
use to find the nodes, and KEY-QUERY is the query to use to find the
key within each node."
  (lambda ()
    (seq-map (lambda (x) (cons (string-join (car x) ".") (cadr x)))
             (tree-surgeon--kv-nesting-nodes lang value-query key-query))))

(provide 'tree-surgeon-imenu-utils)
;;; tree-surgeon-imenu-utils.el ends here
