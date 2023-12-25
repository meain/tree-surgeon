;;; tree-surgeon-go-error.el -- Generate go error returns automatically -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a function to generate go error returns
;; automatically.  The parsing of return types are done via
;; tree-sitter.

;;; Code:
(require 'treesit)

(defgroup tree-surgeon-go-error nil
  "Generate go error returns automatically."
  :group 'tree-surgeon)

(defvar tree-surgeon-go-error--func-query
  "(function_declaration) @f
     (method_declaration) @f")

(defvar tree-surgeon-go-error--params-query
  "result: (type_identifier) @p
     result: (parameter_list (parameter_declaration type: (qualified_type) @p))
     result: (parameter_list (parameter_declaration type: (type_identifier) @p))
     result: (parameter_list (parameter_declaration type: (pointer_type) @p))")

(defvar tree-surgeon-go-error-format-simple (lambda () "err"))
(defvar tree-surgeon-go-error-format-with-wrap
  (lambda ()
    (let ((wrap-message (read-string "Wrap message: ")))
      (concat "fmt.Errorf(\"" wrap-message ": %v\", err)"))))
(defcustom tree-surgeon-go-error-format-default
  tree-surgeon-go-error-format-simple
  "The format for the error return value."
  :type 'function
  :group 'tree-surgeon-go-error)

(defun tree-surgeon-go-error--default-returns (type errformat)
  "Return the default return value for given `TYPE' and `ERRFORMAT'."
  (pcase type
    ("error" (funcall errformat))
    ("bool" "false")
    ("string" "\"\"")
    ("byte" "0") ("rune" "0")
    ("int" "0") ("int32" "0") ("int64" "0") ("uint64" "0")
    ("float32" "0.0") ("float64" "0.0")
    ("chan" "nil")
    ("interface" "nil")
    ("map" "nil")
    ("func" "nil")
    ("io.Reader" "nil")
    ("io.Writer" "nil")
    ("io.ReadCloser" "nil")
    ("io.WriteCloser" "nil")
    ("io.ReadWriteCloser" "nil")
    ((pred (string-prefix-p "<-")) "nil")
    ((pred (string-prefix-p "[")) "nil")
    ((pred (string-match " ")) nil) ; for situations with return name
    ((pred (string-prefix-p "*")) (concat (replace-regexp-in-string "\*" "&" type) "{}"))
    (_ (concat type "{}"))))

(defun tree-surgeon-go-error--get-return-string (errformat)
  "Return the string value for the return statement."
  (when-let* ((root (treesit-buffer-root-node))
              (f-nodes (treesit-query-capture root tree-surgeon-go-error--func-query))
              (f-node
               (cdr (car (sort
                          (cl-remove-if (lambda (x)
                                          (or (< (point) (treesit-node-start (cdr x)))
                                              (> (point) (treesit-node-end (cdr x)))))
                                        f-nodes)
                          (lambda (a b) (<
                                         (- (point) (treesit-node-start (cdr a)))
                                         (- (point) (treesit-node-start (cdr b))))))))))

    ;; TODO: find a way to drop the empty space at the end for
    ;; functions without any return arguments
    (concat "return "
            (string-join (mapcar
                          (lambda (param)
                            (let ((type (treesit-node-text (cdr param) t)))
                              (tree-surgeon-go-error--default-returns type errformat)))
                          (treesit-query-capture f-node tree-surgeon-go-error--params-query))
                         ", "))))

;;;###autoload
(defun tree-surgeon-go-error (&optional errformat)
  "Insert the return statement.
Pass in an `ERRFORMAT' of you want to override the default."
  (interactive)
  (let ((return-string (tree-surgeon-go-error--get-return-string
                        (or errformat tree-surgeon-go-error-format-default))))
    (if return-string
        (let ((start (progn (end-of-line) (newline) (point))))
          (insert "if err != nil {\n")
          (insert return-string)
          (insert "\n}")
          (indent-region start (point)))
      (message "Could not compute return statement"))))

(provide 'tree-surgeon-go-error)
;;; tree-surgeon-go-error.el ends here