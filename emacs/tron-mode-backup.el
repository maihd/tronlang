(define-abbrev-table 'tron-mode-abbrev-table ()
  "Abbrev table for Tron mode.")

(defvar tron--mode-syntax-table
  (let ((table (make-syntax-table))
        (i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (modify-syntax-entry ?\s "    " table)
    ;; Non-break space acts as whitespace.
    (modify-syntax-entry ?\xa0 "    " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\f "    " table)
    (modify-syntax-entry ?\n ">   " table)
    ;; This is probably obsolete since nowadays such features use overlays.
    ;; ;; Give CR the same syntax as newline, for selective-display.
    ;; (modify-syntax-entry ?\^m ">   " table)
    (modify-syntax-entry ?\; "<   " table)
    (modify-syntax-entry ?` "'   " table)
    (modify-syntax-entry ?' "'   " table)
    (modify-syntax-entry ?, "'   " table)
    (modify-syntax-entry ?@ "_ p" table)
    ;; Used to be singlequote; changed for flonums.
    (modify-syntax-entry ?. "_   " table)
    (modify-syntax-entry ?# "'   " table)
    (modify-syntax-entry ?\" "\"    " table)
    (modify-syntax-entry ?\\ "\\   " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Parent syntax table used in Lisp modes.")

(defvar tron-mode-syntax-table
  (let ((table (make-syntax-table tron--mode-syntax-table)))
    (modify-syntax-entry ?\[ "_   " table)
    (modify-syntax-entry ?\] "_   " table)
    (modify-syntax-entry ?# "' 14" table)
    (modify-syntax-entry ?| "\" 23bn" table)
    table)
  "Syntax table used in `tron-mode'.")

(eval-and-compile
  (defconst tron-mode-symbol-regexp "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))

(defvar tron-imenu-generic-expression
  (list
   (list nil
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '("function" "funcall" "progn")
                              t))
			   "\\s-+\\(" tron-mode-symbol-regexp "\\)"))
	 2)
   (list (purecopy "Variables")
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '("var" "set!" "get!" "object!" "array!")
                              t))
			   "\\s-+\\(" tron-mode-symbol-regexp "\\)"))
	 2)
   ;; For `defvar'/`defvar-local', we ignore (defvar FOO) constructs.
   (list (purecopy "Variables")
	 (purecopy (concat "^\\s-*(var\\(?:-local\\)?\\s-+\\("
                           tron-mode-symbol-regexp "\\)"
			   "[[:space:]\n]+[^)]"))
	 1)
   (list (purecopy "Types")
	 (purecopy (concat "^\\s-*("
			   (eval-when-compile
			     (regexp-opt
			      '()
                              t))
			   "\\s-+'?\\(" tron-mode-symbol-regexp "\\)"))
	 2))

  "Imenu generic expression for Lisp mode.  See `imenu-generic-expression'.")

(defvar tron-doc-string-elt-property 'doc-string-elt
  "The symbol property that holds the docstring position info.")

(defconst tron-prettify-symbols-alist '(("lambda"  . ?λ))
  "Alist of symbol/\"pretty\" characters to be displayed.")

;;;; Font-lock support.

(defun tron--match-hidden-arg (limit)
  (let ((res nil))
    (while
        (let ((ppss (parse-partial-sexp (line-beginning-position)
                                        (line-end-position)
                                        -1)))
          (skip-syntax-forward " )")
          (if (or (>= (car ppss) 0)
                  (looking-at ";\\|$"))
              (progn
                (forward-line 1)
                (< (point) limit))
            (looking-at ".*")           ;Set the match-data.
	    (forward-line 1)
            (setq res (point))
            nil)))
    res))

(defun tron--non-funcall-position-p (pos)
  "Heuristically determine whether POS is an evaluated position."
  (save-match-data
    (save-excursion
      (ignore-errors
        (goto-char pos)
        (or (eql (char-before) ?\')
            (let* ((ppss (syntax-ppss))
                   (paren-posns (nth 9 ppss))
                   (parent
                    (when paren-posns
                      (goto-char (car (last paren-posns))) ;(up-list -1)
                      (cond
                       ((ignore-errors
                          (and (eql (char-after) ?\()
                               (when (cdr paren-posns)
                                 (goto-char (car (last paren-posns 2)))
                                 (looking-at "(\\_<let\\*?\\_>"))))
                        (goto-char (match-end 0))
                        'let)
                       ((looking-at
                         (rx "("
                             (group-n 1 (+ (or (syntax w) (syntax _))))
                             symbol-end))
                        (prog1 (intern-soft (match-string-no-properties 1))
                          (goto-char (match-end 1))))))))
              (or (eq parent 'declare)
                  (and (eq parent 'let)
                       (progn
                         (forward-sexp 1)
                         (< pos (point))))
                  (and (eq parent 'condition-case)
                       (progn
                         (forward-sexp 2)
                         (< (point) pos))))))))))

(defun tron--match-keyword (limit)
  ;; FIXME: Move to elisp-mode.el.
  (catch 'found
    (while (re-search-forward
            (eval-when-compile
              (concat "(\\(" tron-mode-symbol-regexp "\\)\\_>"))
            limit t)
      (let ((sym (intern-soft (match-string 1))))
	(when (or (special-form-p sym)
		  (and (macrop sym)
                       (not (get sym 'no-font-lock-keyword))
                       (not (tron--non-funcall-position-p
                             (match-beginning 0)))))
	  (throw 'found t))))))

(defmacro let-when-compile (bindings &rest body)
  "Like `let*', but allow for compile time optimization.
Use BINDINGS as in regular `let*', but in BODY each usage should
be wrapped in `eval-when-compile'.
This will generate compile-time constants from BINDINGS."
  (declare (indent 1) (debug let))
  (letrec ((loop
            (lambda (bindings)
              (if (null bindings)
                  (macroexpand-all (macroexp-progn body)
                                   macroexpand-all-environment)
                (let ((binding (pop bindings)))
                  (cl-progv (list (car binding))
                      (list (eval (nth 1 binding) t))
                    (funcall loop bindings)))))))
    (funcall loop bindings)))

(defun tron--font-lock-backslash ()
  (let* ((beg0 (match-beginning 0))
         (end0 (match-end 0))
         (ppss (save-excursion (syntax-ppss beg0))))
    (and (nth 3 ppss)                  ;Inside a string.
         (not (nth 5 ppss))            ;The \ is not itself \-escaped.
         ;; Don't highlight the \( introduced because of
         ;; `open-paren-in-column-0-is-defun-start'.
         (not (eq ?\n (char-before beg0)))
         (equal (ignore-errors
                  (car (read-from-string
                        (format "\"%s\""
                                (buffer-substring-no-properties
                                 beg0 end0)))))
                (buffer-substring-no-properties (1+ beg0) end0))
         `(face ,font-lock-warning-face
                help-echo "This \\ has no effect"))))

(let-when-compile
    ((tron-fdefs '("function" "funcall" "progn"))
     (tron-vdefs '("var" "set!" "array!" "object!"))
     (tron-kw '("var" "set!" "get!" "array!" "object!" "funcall"
		"function" "progn" "js-code!"))
     (tron-tdefs '())
     (tron-errs '()))
  (let ((vdefs (eval-when-compile tron-vdefs))
        (tdefs (eval-when-compile tron-tdefs))
	(tron-defs-re (eval-when-compile
			(regexp-opt (append tron-fdefs tron-vdefs) t)))
	(tron-errs-re (eval-when-compile
			(regexp-opt tron-errs t))))
    (dolist (v vdefs)
      (put (intern v) 'tron-define-type 'var))
    (dolist (v tdefs)
      (put (intern v) 'tron-define-type 'type))
    
    (defconst tron-font-lock-keywords-1
      `( ;; Definitions.
        (,(concat "(" tron-defs-re "\\_>"
                  ;; Any whitespace and defined object.
                  "[ \t']*"
                  "\\(([ \t']*\\)?" ;; An opening paren.
                  "\\(\\(set!\\)[ \t]+" tron-mode-symbol-regexp
                  "\\|" tron-mode-symbol-regexp "\\)?")
          (1 font-lock-keyword-face)
          (3 (let ((type (get (intern-soft (match-string 1))
			      'tron-define-type)))
               (cond ((eq type 'var) font-lock-variable-name-face)
                     ((eq type 'type) font-lock-type-face)
                     ;; If match-string 2 is non-nil, we encountered a
                     ;; form like (defalias (intern (concat s "-p"))),
                     ;; unless match-string 4 is also there.  Then its a
                     ;; defmethod with (setf foo) as name.
                     ((or (not (match-string 2)) ;; Normal defun.
                          (and (match-string 2)  ;; Setf method.
                               (match-string 4)))
                      font-lock-function-name-face)))
             nil t))
        ;; Emacs Lisp autoload cookies.  Supports the slightly different
        ;; forms used by mh-e, calendar, etc.
        ("^;;;###\\([-a-z]*autoload\\)" 1 font-lock-warning-face prepend))
      "Subdued level highlighting for Emacs Lisp mode.")

    (defconst tron-font-lock-keywords-2
      (append
       tron-font-lock-keywords-1
       `( ;; Regexp negated char group.
         ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)
         ;; Erroneous structures.
         (,(concat "(" tron-errs-re "\\_>")
          (1 font-lock-warning-face))
         ;; Control structures.  Common Lisp forms.
         (tron--match-keyword . 1)
         ;; Exit/Feature symbols as constants.
         (,(concat "(\\(catch\\|throw\\|featurep\\|provide\\|require\\)\\_>"
                   "[ \t']*\\(" tron-mode-symbol-regexp "\\)?")
           (1 font-lock-keyword-face)
           (2 font-lock-constant-face nil t))
         ;; Words inside \\[] tend to be for `substitute-command-keys'.
         (,(concat "\\\\\\\\\\[\\(" tron-mode-symbol-regexp "\\)\\]")
          (1 font-lock-constant-face prepend))
         ;; Ineffective backslashes (typically in need of doubling).
         ("\\(\\\\\\)\\([^\"\\]\\)"
          (1 (tron--font-lock-backslash) prepend))
         ;; Words inside ‘’ and `' tend to be symbol names.
         (,(concat "[`‘]\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)"
                   tron-mode-symbol-regexp "\\)['’]")
          (1 font-lock-constant-face prepend))
         ;; Constant values.
         (,(concat "\\_<:" tron-mode-symbol-regexp "\\_>")
          (0 font-lock-builtin-face))
         ;; ELisp and CLisp `&' keywords as types.
         (,(concat "\\_<\\&" tron-mode-symbol-regexp "\\_>")
          . font-lock-type-face)
         ;; ELisp regexp grouping constructs
         (,(lambda (bound)
             (catch 'found
               ;; The following loop is needed to continue searching after matches
               ;; that do not occur in strings.  The associated regexp matches one
               ;; of `\\\\' `\\(' `\\(?:' `\\|' `\\)'.  `\\\\' has been included to
               ;; avoid highlighting, for example, `\\(' in `\\\\('.
               (while (re-search-forward "\\(\\\\\\\\\\)\\(?:\\(\\\\\\\\\\)\\|\\((\\(?:\\?[0-9]*:\\)?\\|[|)]\\)\\)" bound t)
                 (unless (match-beginning 2)
                   (let ((face (get-text-property (1- (point)) 'face)))
                     (when (or (and (listp face)
                                    (memq 'font-lock-string-face face))
                               (eq 'font-lock-string-face face))
                       (throw 'found t)))))))
           (1 'font-lock-regexp-grouping-backslash prepend)
           (3 'font-lock-regexp-grouping-construct prepend))
         (tron--match-hidden-arg
          (0 '(face font-lock-warning-face
               help-echo "Hidden behind deeper element; move to another line?")))
         ))
      "Gaudy level highlighting for Tron mode.")))

(defvar tron-font-lock-keywords lisp-font-lock-keywords-1
"Default expressions to highlight in Tron Lisp mode.")

(defun tron-string-in-doc-position-p (listbeg startpos)
   "Return true if a doc string may occur at STARTPOS inside a list.
LISTBEG is the position of the start of the innermost list
containing STARTPOS."
  (let* ((firstsym (and listbeg
                        (save-excursion
                          (goto-char listbeg)
                          (and (looking-at
                                (eval-when-compile
                                  (concat "([ \t\n]*\\("
                                          tron-mode-symbol-regexp "\\)")))
                               (match-string 1)))))
         (docelt (and firstsym
                      (function-get (intern-soft firstsym)
                                    tron-doc-string-elt-property))))
    (and docelt
         ;; It's a string in a form that can have a docstring.
         ;; Check whether it's in docstring position.
         (save-excursion
           (when (functionp docelt)
             (goto-char (match-end 1))
             (setq docelt (funcall docelt)))
           (goto-char listbeg)
           (forward-char 1)
           (condition-case nil
               (while (and (> docelt 0) (< (point) startpos)
                           (progn (forward-sexp 1) t))
                 (setq docelt (1- docelt)))
             (error nil))
           (and (zerop docelt) (<= (point) startpos)
                (progn (forward-comment (point-max)) t)
                (= (point) startpos))))))

(defun tron-string-after-doc-keyword-p (listbeg startpos)
  "Return true if `:documentation' symbol ends at STARTPOS inside a list.
LISTBEG is the position of the start of the innermost list
containing STARTPOS."
  (and listbeg                          ; We are inside a Lisp form.
       (save-excursion
         (goto-char startpos)
         (ignore-errors
           (progn (backward-sexp 1)
                  (looking-at ":documentation\\_>"))))))

(defun tron-font-lock-syntactic-face-function (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
Lisp font lock syntactic face function."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let ((listbeg (nth 1 state)))
            (if (or (tron-string-in-doc-position-p listbeg startpos)
                    (tron-string-after-doc-keyword-p listbeg startpos))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun tron-adaptive-fill ()
  "Return fill prefix found at point.
Value for `adaptive-fill-function'."
  ;; Adaptive fill mode gets the fill wrong for a one-line paragraph made of
  ;; a single docstring.  Let's fix it here.
  (if (looking-at "\\s-+\"[^\n\"]+\"\\s-*$") ""))

(defun tron-mode-variables (&optional tron-syntax keywords-case-insensitive)
  "Common initialization routine for lisp modes.
The LISP-SYNTAX argument is used by code in inf-lisp.el and is
\(uselessly) passed from pp.el, chistory.el, gnus-kill.el and
score-mode.el.  KEYWORDS-CASE-INSENSITIVE non-nil means that for
font-lock keywords will not be case sensitive."
  (when tron-syntax
    (set-syntax-table tron-mode-syntax-table))
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'tron-fill-paragraph)
  (setq-local adaptive-fill-function #'tron-adaptive-fill)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  ;;  I believe that newcomment's auto-fill code properly deals with it  -stef
  ;;(set (make-local-variable 'adaptive-fill-mode) nil)
  (setq-local indent-line-function 'tron-indent-line)
  (setq-local indent-region-function 'tron-indent-region)
  (setq-local comment-indent-function #'tron-comment-indent)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
  (setq-local outline-level 'tron-outline-level)
  (setq-local add-log-current-defun-function #'tron-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)		;default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local imenu-generic-expression tron-imenu-generic-expression)
  (setq-local multibyte-syntax-as-symbol t)
  ;; (setq-local syntax-begin-function 'beginning-of-defun)  ;;Bug#16247.
  (setq font-lock-defaults
	`('(tron-font-lock-keywords
            tron-font-lock-keywords-1
            tron-font-lock-keywords-2)
	  nil ,keywords-case-insensitive nil nil
	  (font-lock-mark-block-function . mark-defun)
          (font-lock-extra-managed-props help-echo)
	  (font-lock-syntactic-face-function
	   . tron-font-lock-syntactic-face-function)))
  (setq-local prettify-symbols-alist tron-prettify-symbols-alist)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil))

(defun tron-outline-level ()
  "Lisp mode `outline-level' function."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (if (looking-at "(\\|;;;###autoload")
	1000
      len)))

(defun tron-current-defun-name ()
  "Return the name of the defun at point, or nil."
  (save-excursion
    (let ((location (point)))
      ;; If we are now precisely at the beginning of a defun, make sure
      ;; beginning-of-defun finds that one rather than the previous one.
      (or (eobp) (forward-char 1))
      (beginning-of-defun)
      ;; Make sure we are really inside the defun found, not after it.
      (when (and (looking-at "\\s(")
		 (progn (end-of-defun)
			(< location (point)))
		 (progn (forward-sexp -1)
			(>= location (point))))
	(if (looking-at "\\s(")
	    (forward-char 1))
	;; Skip the defining construct name, typically "defun" or
	;; "defvar".
	(forward-sexp 1)
	;; The second element is usually a symbol being defined.  If it
	;; is not, use the first symbol in it.
	(skip-chars-forward " \t\n'(")
	(buffer-substring-no-properties (point)
					(progn (forward-sexp 1)
					       (point)))))))

(defvar tron-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; This gets in the way when viewing a Lisp file in view-mode.  As
    ;; long as [backspace] is mapped into DEL via the
    ;; function-key-map, this should remain disabled!!
    ;;;(define-key map [backspace] 'backward-delete-char-untabify)
    map)
  "Keymap for commands shared by all sorts of Tron modes.")

(defcustom tron-mode-hook nil
  "Hook run when entering Lisp mode."
  :options '(imenu-add-menubar-index)
  :type 'hook
  :group 'tron)

;;; Generic Lisp mode.

(defvar tron-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Tron")))
    (set-keymap-parent map tron-mode-shared-map)
    map)
  "Keymap for ordinary Lisp mode.
All commands in `tron-mode-shared-map' are inherited by this map.")

(define-derived-mode tron-mode prog-mode "Tron"
  "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{tron-mode-map}"
  (tron-mode-variables nil)
  (setq-local find-tag-default-function 'tron-find-tag-default)
  (setq-local comment-start-skip
	      "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq imenu-case-fold-search t))

(defun tron-find-tag-default ()
  (let ((default (find-tag-default)))
    (when (stringp default)
      (if (string-match ":+" default)
          (substring default (match-end 0))
	default))))


(defun tron-comment-indent ()
  "Like `comment-indent-default', but don't put space after open paren."
  (or (when (looking-at "\\s<\\s<")
        (let ((pt (point)))
          (skip-syntax-backward " ")
          (if (eq (preceding-char) ?\()
              (cons (current-column) (current-column))
            (goto-char pt)
            nil)))
      (comment-indent-default)))

(define-obsolete-function-alias 'tron-mode-auto-fill 'do-auto-fill "23.1")

(defcustom tron-indent-offset nil
  "If non-nil, indent second line of expressions that many more columns."
  :group 'tron
  :type '(choice (const nil) integer))
(put 'tron-indent-offset 'safe-local-variable
     (lambda (x) (or (null x) (integerp x))))

(defcustom tron-indent-function 'tron-indent-function
  "A function to be called by `calculate-tron-indent'.
It indents the arguments of a Lisp function call.  This function
should accept two arguments: the indent-point, and the
`parse-partial-sexp' state at that position."
  :type 'function
  :group 'tron)

(defun tron-ppss (&optional pos)
  "Return Parse-Partial-Sexp State at POS, defaulting to point.
Like `syntax-ppss' but includes the character address of the last
complete sexp in the innermost containing list at position
2 (counting from 0).  This is important for lisp indentation."
  (unless pos (setq pos (point)))
  (let ((pss (syntax-ppss pos)))
    (if (nth 9 pss)
        (let ((sexp-start (car (last (nth 9 pss)))))
          (parse-partial-sexp sexp-start pos nil nil
			      (syntax-ppss sexp-start)))
      pss)))

(cl-defstruct (tron-indent-state
               (:constructor nil)
               (:constructor tron-indent-initial-state
                             (&aux (ppss (tron-ppss))
                                   (ppss-point (point))
                                   (stack (make-list (1+ (car ppss))
						     nil)))))
  stack ;; Cached indentation, per depth.
  ppss
  ppss-point)

(defun tron-indent-calc-next (state)
  "Move to next line and return calculated indent for it.
STATE is updated by side effect, the first state should be
created by `lisp-indent-initial-state'.  This function may move
by more than one line to cross a string literal."
  (pcase-let* (((cl-struct tron-indent-state
                           (stack indent-stack) ppss ppss-point)
                state)
               (indent-depth (car ppss)) ; Corresponding to indent-stack.
               (depth indent-depth))
    ;; Parse this line so we can learn the state to indent the
    ;; next line.
    (while (let ((last-sexp (nth 2 ppss)))
             (setq ppss (parse-partial-sexp
                         ppss-point (progn (end-of-line) (point))
                         nil nil ppss))
             ;; Preserve last sexp of state (position 2) for
             ;; `calculate-lisp-indent', if we're at the same depth.
             (if (and (not (nth 2 ppss)) (= depth (car ppss)))
                 (setf (nth 2 ppss) last-sexp)
               (setq last-sexp (nth 2 ppss)))
             (setq depth (car ppss))
             ;; Skip over newlines within strings.
             (nth 3 ppss))
      (let ((string-start (nth 8 ppss)))
        (setq ppss (parse-partial-sexp (point) (point-max)
                                       nil nil ppss 'syntax-table))
        (setf (nth 2 ppss) string-start) ; Finished a complete string.
        (setq depth (car ppss)))
      (setq ppss-point (point)))
    (setq ppss-point (point))
    (let* ((depth-delta (- depth indent-depth)))
      (cond ((< depth-delta 0)
             (setq indent-stack (nthcdr (- depth-delta) indent-stack)))
            ((> depth-delta 0)
             (setq indent-stack (nconc (make-list depth-delta nil)
                                       indent-stack)))))
    (prog1
        (let (indent)
          (cond ((= (forward-line 1) 1) nil)
                ((car indent-stack))
                ((integerp (setq indent (calculate-tron-indent ppss)))
                 (setf (car indent-stack) indent))
                ((consp indent)       ; (COLUMN CONTAINING-SEXP-START)
                 (car indent))
                ;; This only happens if we're in a string.
                (t (error "This shouldn't happen"))))
      (setf (tron-indent-state-stack state) indent-stack)
      (setf (tron-indent-state-ppss-point state) ppss-point)
      (setf (tron-indent-state-ppss state) ppss))))

(defun tron-indent-region (start end)
  "Indent region as Lisp code, efficiently."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (beginning-of-line)
    ;; The default `indent-region-line-by-line' doesn't hold a running
    ;; parse state, which forces each indent call to reparse from the
    ;; beginning.  That has O(n^2) complexity.
    (let* ((parse-state (tron-indent-initial-state))
           (pr (unless (minibufferp)
                 (make-progress-reporter "Indenting region..."
					 (point)
					 end))))
      (let ((ppss (tron-indent-state-ppss parse-state)))
        (unless (or (and (bolp) (eolp)) (nth 3 ppss))
          (lisp-indent-line (calculate-tron-indent ppss))))
      (let ((indent nil))
        (while (progn (setq indent (tron-indent-calc-next parse-state))
                      (< (point) end))
          (unless (or (and (bolp) (eolp)) (not indent))
            (tron-indent-line indent))
          (and pr (progress-reporter-update pr (point)))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(defun tron-indent-line (&optional indent)
  "Indent current line as Lisp code."
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (progn (beginning-of-line)
                       (or indent (calculate-tron-indent (tron-ppss))))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at "\\s<\\s<\\s<"))
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
        ;; FIXME: inconsistency: comment-indent moves ;;; to column 0.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at "\\s<\\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (forward-char -1))
	(if (listp indent) (setq indent (car indent)))
        (indent-line-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defvar calculate-tron-indent-last-sexp)

(defun calculate-tron-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.
PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.
The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-tron-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
		  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-tron-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-tron-indent-last-sexp
		 (> calculate-tron-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-tron-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-tron-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-tron-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-tron-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-tron-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-tron-indent-last-sexp
					      0
					      t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; `calculate-tron-indent-last-sexp'.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-tron-indent-last-sexp)
		 (beginning-of-line)
		 (parse-partial-sexp (point)
				     calculate-tron-indent-last-sexp
				     0
				     t)
		 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
	       nil)
              ((and (integerp tron-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) tron-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-tron-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and tron-indent-function
                     (not retry)
                     (funcall tron-indent-function indent-point state))
                ;; If the function has no special alignment
		;; or it does not apply to this argument,
		;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-tron-indent-last-sexp)
		       ;; Handle prefix characters and whitespace
		       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back
					"^[ \t]*\\|([ \t]+"
					(line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp)
						(point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-tron-indent-last-sexp (point)))
                     (> calculate-tron-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp
			   (point)
			   calculate-tron-indent-last-sexp
			   0
			   t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-tron-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun tron-indent-function (indent-point state)
  "This function is the normal value of the variable `tron-indent-function'.
The function `calculate-tron-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `tron-indent-function' (or the deprecated `tron-indent-hook'),
it specifies how to indent.  The property value can be:
* `function', meaning indent `function'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `tron-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-tron-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-tron-indent-last-sexp))
		(progn (goto-char calculate-tron-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-tron-indent-last-sexp
					   0
					   t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (function-get (intern-soft function)
                                       'tron-indent-function)
			 (get (intern-soft function) 'tron-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(= (length function) 8)
			(string-match "\\`function" function)))
	       (tron-indent-defform state indent-point))
	      ((integerp method)
	       (tron-indent-specform method
				     state
				     indent-point
				     normal-indent))
	      (method
		(funcall method indent-point state)))))))

(defcustom tron-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form."
  :group 'tron
  :type 'integer)
(put 'tron-body-indent 'safe-local-variable 'integerp)

(defun tron-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ tron-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lisp-body-indent, else normal indent.  With lisp-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 tron-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun tron-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ tron-body-indent (current-column)))))


;; (put 'progn 'lisp-indent-function 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'progn 'tron-indent-function 0)
(put 'if    'tron-indent-function 0)

(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let* ((parse-state (tron-indent-initial-state)))
    ;; We need a marker because we modify the buffer
    ;; text preceding endpos.
    (setq endpos (copy-marker
                  (if endpos endpos
                    ;; Get error now if we don't have a complete sexp
                    ;; after point.
                    (save-excursion (forward-sexp 1) (point)))))
    (save-excursion
      (while (let ((indent (tron-indent-calc-next parse-state))
                   (ppss (tron-indent-state-ppss parse-state)))
               ;; If the line contains a comment indent it now with
               ;; `indent-for-comment'.
               (when (and (nth 4 ppss) (<= (nth 8 ppss) endpos))
                 (save-excursion
                   (goto-char (tron-indent-state-ppss-point parse-state))
                   (indent-for-comment)
                   (setf (tron-indent-state-ppss-point parse-state)
                         (tron-end-position))))
               (when (< (point) endpos)
                 ;; Indent the next line, unless it's blank, or just a
                 ;; comment (we will `indent-for-comment' the latter).
                 (skip-chars-forward " \t")
                 (unless (or (eolp) (not indent)
                             (eq (char-syntax (char-after)) ?<))
                   (indent-line-to indent))
                 t))))
    (move-marker endpos nil)))

(defun indent-pp-sexp (&optional arg)
  "Indent each line of the list starting just after point, or prettyprint it.
A prefix argument specifies pretty-printing."
  (interactive "P")
  (if arg
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (progn (forward-sexp 1) (point)))
          (pp-buffer)
          (goto-char (point-max))
          (if (eq (char-before) ?\n)
              (delete-char -1)))))
  (indent-sexp))

;;;; Tron paragraph filling commands.

(defun tron-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments and docstrings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Since fill-comment-paragraph returned nil, that means we're not in
      ;; a comment: Point is on a program line; we are interested
      ;; particularly in docstring lines.
      ;;
      ;; We bind `paragraph-start' and `paragraph-separate' temporarily.  They
      ;; are buffer-local, but we avoid changing them so that they can be set
      ;; to make `forward-paragraph' and friends do something the user wants.
      ;;
      ;; `paragraph-start': The `(' in the character alternative and the
      ;; left-singlequote plus `(' sequence after the \\| alternative prevent
      ;; sexps and backquoted sexps that follow a docstring from being filled
      ;; with the docstring.  This setting has the consequence of inhibiting
      ;; filling many program lines that are not docstrings, which is sensible,
      ;; because the user probably asked to fill program lines by accident, or
      ;; expecting indentation (perhaps we should try to do indenting in that
      ;; case).  The `;' and `:' stop the paragraph being filled at following
      ;; comment lines and at keywords (e.g., in `defcustom').  Left parens are
      ;; escaped to keep font-locking, filling, & paren matching in the source
      ;; file happy.  The `:' must be preceded by whitespace so that keywords
      ;; inside of the docstring don't start new paragraphs (Bug#7751).
      ;;
      ;; `paragraph-separate': A clever regexp distinguishes the first line of
      ;; a docstring and identifies it as a paragraph separator, so that it
      ;; won't be filled.  (Since the first line of documentation stands alone
      ;; in some contexts, filling should not alter the contents the author has
      ;; chosen.)  Only the first line of a docstring begins with whitespace
      ;; and a quotation mark and ends with a period or (rarely) a comma.
      ;;
      ;; The `fill-column' is temporarily bound to
      ;; `emacs-lisp-docstring-fill-column' if that value is an integer.
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
	    (paragraph-separate
	     (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (if (and (integerp tron-docstring-fill-column)
                                  (derived-mode-p 'emacs-lisp-mode))
                             tron-docstring-fill-column
                           fill-column)))
	(fill-paragraph justify))
      ;; Never return nil.
      t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.
Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

(add-to-list 'auto-mode-alist '("\\.tron\\'" . tron-mode))

(provide 'tron-mode)

;;; tron-mode.el ends here
