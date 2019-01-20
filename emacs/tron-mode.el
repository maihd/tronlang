(require 'lisp-mode)
(require 'scheme)

(defvar tron-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are Scheme-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar tron-mode-abbrev-table nil)
(define-abbrev-table 'scheme-mode-abbrev-table ())

(defvar tron-imenu-generic-expression
  '((nil
     "^(func\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
    ("Types"
     "^(define-class\\s-+(?\\(\\sw+\\)" 1)
    ("Macros"
     "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Scheme mode.  See `imenu-generic-expression'.")

(defun tron-mode-variables ()
  (set-syntax-table tron-mode-syntax-table)
  (setq local-abbrev-table tron-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'tron-indent-function)
  (setq mode-line-process '("" tron-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression tron-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'tron-syntax-propertize)
  (setq font-lock-defaults
        '((tron-font-lock-keywords
           tron-font-lock-keywords-1
	   tron-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (setq-local prettify-symbols-alist lisp-prettify-symbols-alist)
  (setq-local lisp-doc-string-elt-property 'tron-doc-string-elt))

(defvar tron-mode-line-process "")

(defvar tron-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "Tron")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar tron] (cons "Tron" map))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Scheme mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmuscheme
(defun tron-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(define-derived-mode tron-mode scheme-mode "Tron"
  "Major mode for editing Tron code."
  (tron-mode-variables))

(defgroup tron nil
  "Editing Scheme code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'scheme)

(defcustom tron-mode-hook nil
  "Normal hook run when entering `tron-mode'.
See `run-hooks'."
  :type 'hook
  :group 'tron)

;; This is shared by cmuscheme and xscheme.
(defcustom tron-program-name "tron"
  "Program invoked by the `run-tron' command."
  :type 'string
  :group 'tron)

(defconst tron-font-lock-keywords-1
  (eval-when-compile
    (list
     (list (concat "(\\(def\\*?\\("
                   ;; Function names.
                   "\\(n\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(macro\\)\\|"
                   ;; Variable names
                   "\\(var\\)\\|"
                   ;; Class names.
                   "\\(type\\)"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   ;; The "(*" is for curried definitions, e.g.,
                   ;;  (def sum (a b) (+ a b))
                   "[ \t]*(*"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                     ((match-beginning 5) font-lock-variable-name-face)
                     (t font-lock-type-face))
               nil t))
     ))
  "Subdued expressions to highlight in Tron modes.")

(defconst tron-font-lock-keywords-2
  (append
   tron-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (list (concat "(\\(let\\|lambda\\|call\\|progn\\|new\\|import\\|export"
                    "\\|else\\|for-each\\|if"
                    "\\|and\\|or\\|not\\|bit-and\\|bit-or\\|bit-xor"
                    "\\|array!\\|object!\\|js-code!"
                    "\\|set!\\|get!\\|quote\\|unquote\\|quasiquote"
                    "\\|pipe\\|typeof\\|instanceof\\|this"
                    "\\)\\>"
                    "[ \t]*(*"
                    "\\(\\sw+\\)?")
            '(1 font-lock-keyword-face))
      
      ;;
      ;; Operators
      (list (concat "("
                    (regexp-opt '("+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "<<" ">>") t)
                    "\\>"
                    "[ \t]*(*"
                    "\\(\\sw+\\)?")
            '(1 font-lock-keyword-face))
      
      ;;
      ;; It wouldn't be Scheme w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      
      ;;
      '("\\<<\\sw+>\\>" . font-lock-type-face)

      ;;
      ;; Tron `#:' keywords as builtins.
      '("\\<#:\\sw+\\>" . font-lock-builtin-face)
      
      ;;
      ;; Tron `#rx' regexps as builtins.
      '("\\<#rx\\sw+\\>" . font-lock-builtin-face)

      ;;
      ;; Tron `#x' hexanumber as builtins.
      '("\\<#x\\sw+\\>" . font-lock-builtin-face)
      
      ;;
      ;; Tron `#\' character as builtins.
      '("\\<#\\\\sw+\\>" . font-lock-builtin-face)
      
      ;; R6RS library declarations.
      '("(\\(\\<library\\>\\)\\s-*(?\\(\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-type-face))
      )))
   "Gaudy expressions to highlight in Tron modes.")
  
(defvar tron-font-lock-keywords tron-font-lock-keywords-1
  "Default expressions to highlight in Tron modes.")

(defconst tron-sexp-comment-syntax-table
  (let ((st (make-syntax-table tron-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'tron-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'tron-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun tron-syntax-propertize (beg end)
  (goto-char beg)
  (tron-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (tron-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

(defun tron-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
	(scan-error (goto-char end))))))


(defvar calculate-lisp-indent-last-sexp)


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun tron-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:
i) it checks for a non-nil value of the property `scheme-indent-function'
\(or the deprecated `scheme-indent-hook'), rather than `lisp-indent-function'.
ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'function)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function))
		   (member function '("def" "defmacro" "progn"
                              "let" "lambda" "call" "set!"
                              "promise!" "array!" "object!")))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
                (funcall method state indent-point normal-indent)))))))


(defun tron-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'scheme-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'scheme-indent-function 0)
(put 'case 'scheme-indent-function 1)
(put 'delay 'scheme-indent-function 0)
(put 'do 'scheme-indent-function 2)
(put 'lambda 'scheme-indent-function 1)
(put 'let 'scheme-indent-function 'scheme-let-indent)
(put 'let* 'scheme-indent-function 1)
(put 'letrec 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1) ; SRFI 11
(put 'let*-values 'scheme-indent-function 1) ; SRFI 11
(put 'sequence 'scheme-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'scheme-indent-function 1)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2) ; not r5rs
(put 'library 'scheme-indent-function 1) ; R6RS

(put 'call-with-input-file 'scheme-indent-function 1)
(put 'with-input-from-file 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'with-output-to-file 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1) ; r5rs?
(put 'dynamic-wind 'scheme-indent-function 3) ; r5rs?

;; R7RS
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'letrec* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'define-values 'scheme-indent-function 1)
(put 'define-record-type 'scheme-indent-function 1) ;; is 1 correct?
(put 'define-library 'scheme-indent-function 1)

;; SRFI-8
(put 'receive 'scheme-indent-function 2)

(add-to-list 'auto-mode-alist '("\\.tron\\'" . tron-mode))

(provide 'tron-mode)
