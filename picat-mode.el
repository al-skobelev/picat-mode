;;; picat-mode.el --- Picat editing mode

;; This code is written by Aleksandr Skobelev and placed in the
;; Public Domain.  All warranties are disclaimed.

;;; Commentary:

;; The major mode for editing Picat code. It is loozely inspired by
;; prolog-mode and Reinhard Urban's picat-mode.
;;
;; Known-issues:
;; - I have no idea how to handle indentation for do-while loops.

(require 'smie)
(require 'comint)

(defvar picat-mode-version "0.1")

(defgroup picat nil
  "Editing and running Picat files."
  :group 'languages)


(defcustom picat-indent-width 4
  "The indentation width used by the editing buffer."
  :group 'picat
  :type 'integer
  :safe 'integerp)


(defcustom picat-program "picat"
  "*Program invoked by the `run-picat' command."
  :type 'string
  :group 'picat)


(defcustom picat-path nil
  "Path to directory for loading picat modules."
  :type 'string
  :group 'picat)


(defcustom picat-load-hook nil
  "This hook is run when picat is loaded in.
This is a good place to put keybindings."
  :type 'hook
  :group 'picat)

(defcustom picat-mode-hook nil
  "Normal hook for picat mode."
  :type 'hook
  :group 'picat)

(defvar picat-smie-indent-function 'picat-smie-indent
  "Function to put into the `smie-indent-functions' list.")

;;; MODE KEYMAP ---------------------------------------------------------------
(defvar picat-mode-map

  (let ((smap (make-sparse-keymap))
        (map  (make-sparse-keymap "Picat")))

    (define-key smap "\C-c\C-z" 'switch-to-picat)
    (define-key smap "\C-c\C-l" 'picat-load-file)
    (define-key smap "\C-c\C-k" 'picat-compile-load-file)

    (define-key smap "\e\C-q" 'smie-indent-sexp)
    (define-key smap "\t" 'smie-indent-line)
    smap)
  
  "Keymap for Picat mode.")

;;; KEYWORDS, BUILTINS, OPERATORS ---------------------------------------------
(defconst picat-keywords
  '("import" "in" "include" "module"
    "not" "fail" "pass" "true" "false"
    "private" "table" "index"
    "foreach" "while" "do" "else" "elseif"
    "then" "if" "end" "break"
    ; operators as keywords
    "div" "rem" "mod" "in" "notin"))

(defconst picat-operators
  '("=>" "?=>"
    ";" "||" "->" "," "&&"
    "\\not" "\\once" "\\+"
    "\\#<=>" "\\#=>" "\\#\\/" "\\#^" "\\#/\\"
    "=" "!=" ":=" "==" "!==" "\\=" "=.." "\\=="
    "#=" "#!=" "#<" "#=<" "#<=" "#>" "#>=" "#~"
    "@<" "@=<" "@>" "@>="
    "=:=" "<" "<=" "=<" ">" ">="
    "::"  "=.." "++" ".." "\\/" "^" "/\\"
    "+" "-" "*" "/" "//" "/<" "/>" "<<" ">>"
    "~" "**" "." "@"))

(defconst picat-basic-builtins
  '(;; "!=" "!==" "*" "**" "+" "++" "-" "/" "//" "/<" "/>" "/\\" "<"
    ;; "<<" "<=" "=" "=:=" "=<" "==" "=\\=" "=.." ">" ">=" ">>" "@<"
    ;; "@<=" "@=<" "@>" "@>=" "\\/" "^" "~" "\\+"
    "acyclic_term"
    "and_to_list" "append" "apply" "arg" "arity" "array" "ascii_digit"
    "ascii_alpha" "ascii_alpha_digit" "ascii_lowercase" "ascii_uppercase" "atom"
    "atom_chars" "atom_codes" "atomic" "attr_var" "avg" "between"
    "bigint" "bind_vars" "call" "call_cleanup" "catch" "char" "chr" "clear"
    "compare_terms" "compound" "copy_term" "copy_term_shallow" "del"
    "delete" "delete_all" "different_terms" "digit" "div" "dvar" "bool_dvar"
    "dvar_or_int" "fail" "false" "find_all" "findall" "count_all" "first"
    "flatten" "float" "fold" "freeze" "functor" "get"
    "get_attr" "get_global_map" "get_heap_map" "get_heap_map" "get_table_map"
    "ground" "handle_exception" "has_key" "hash_code" "head" "heap_is_empty"
    "heap_pop" "heap_push" "heap_size" "heap_to_list" "heap_top"
    "insert" "insert_all" "insert_ordered" "insert_ordered_no_dup"
    "insert_ordered_down" "insert_ordered_down_no_dup" "int" "integer" "is" "keys"
    "last" "length" "len" "list" "list_to_and" "lowercase" "map" "map_to_list"
    "max" "maxint_small" "maxof" "maxof_inc" "membchk" "member" "min"
    "minint_small" "minof" "minof_inc" "mod" "name" "new_array" "new_min_heap"
    "new_max_heap" "new_list" "new_map" "new_set" "new_struct" "nonvar" "not" "nth"
    "number" "number_chars" "number_codes" "number_vars" "once" "ord"
    "parse_radix_string" "parse_term" "post_event" "post_event_any"
    "post_event_bound" "post_event_dom" "post_event_ins" "prod" "put" "put_attr"
    "real" "reduce" "rem" "remove_dups" "repeat" "reverse" "second" "select" "size"
    "slice" "slice" "sort" "sort_down" "sort_down_remove_dups" "sort_remove_dups"
    "sorted" "sorted_down" "string" "struct" "subsumes" "sum" "tail" "throw"
    "to_array" "to_atom" "to_binary_string" "to_codes" "to_fstring" "to_hex_string"
    "to_integer" "to_int" "to_list" "to_lowercase" "to_number" "to_oct_string"
    "to_radix_string" "to_real" "to_float" "to_string" "to_uppercase" "true"
    "uppercase" "values" "var" "variant" "vars" "zip"))

(defconst picat-io-builtins
  '("at_end_of_stream" "close" "flush" "nl" "open" "peek_byte" "peek_char"
    "print" "printf" "println" "read_atom" "read_byte" "read_char"
    "read_char_code" "read_file_bytes" "read_file_chars" "read_file_codes"
    "read_file_lines" "read_file_terms" "read_file_tokens" "read_int"
    "read_line" "read_number" "read_picat_token" "read_real" "read_term"
    "readln" "write" "write_byte" "write_char" "write_char_code" "writef"
    "writeln"))

(defconst picat-math-builtins
  '("abs" "acos" "acosh" "acot" "acoth" "acsc" "acsch" "asec" "asech" "asin"
    "asinh" "atan" "atan2" "atanh" "ceiling" "cos" "cosh" "cot" "coth" "csc"
    "csch" "e" "even" "exp" "factorial" "floor" "frand" "frand" "gcd" "log"
    "log" "log10" "log2" "modf" "odd" "pi" "pow" "pow_mod" "prime" "primes"
    "rand_max" "random" "random" "random" "random2" "round" "sec" "sech" "sign"
    "sin" "sinh" "sqrt" "tan" "tanh" "to_degrees" "to_radians" "truncate"))

(defconst picat-sys-builtins
  '("abort" "cl" "cl_facts" "cl_facts_table" "command" "compile" "compile_bp"
    "compile_files_to_c" "debug" "exit" "garbage_collect" "halt" "help"
    "initialize_table" "load" "loaded_modules" "nodebug" "nolog" "nospy"
    "notrace" "picat_path" "spy" "statistics" "statistics_all" "time" "time2"
    "time_out" "trace"))

(defconst picat-builtins
  (append picat-basic-builtins
          picat-io-builtins
          picat-math-builtins
          picat-sys-builtins))

(defconst picat-operator-chars "-\\\\#&*+./:<=>?@\\^`~")


(defconst picat-font-lock-defaults
  `(
    ("\\<[0-9]+\\(\\.[0-9]*\\)?\\([Ee][+-]?[0-9]+\\)?\\>" . font-lock-number-face)
    ("0x[a-fA-F0-9]+" . font-lock-number-face)
    (,(regexp-opt picat-builtins 'symbols) . font-lock-builtin-face)
    (,(regexp-opt picat-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt picat-operators t). font-lock-keyword-face)
    ("^\\_<\\([[:lower:]]\\w*\\)\\(([^)]*)\\)?\\(\\(\\.\\)\\|\\(.*=.*\\.?\\)\\)"
     1 font-lock-function-name-face)
    ("\\<$?\\(\\(_\\|[[:upper:]]\\)\\(\\w\\)*\\)\\>" 1 font-lock-variable-name-face)
    ))


;;; SYNTAX TABLE --------------------------------------------------------------
(defvar picat-syntax-alist
  '((?$ . "w")
    (?_ . "w")
    (?' . "w")
    (?% . "<")
    (?\n . ">")
    (?* . ". 23b")
    (?/ . ". 14b")))

(defvar picat-mode-syntax-table
  (let ((st (make-syntax-table)))
    (dolist (entry picat-syntax-alist)
      (modify-syntax-entry (car entry) (cdr entry)  st))
    st))

;;; SMIE GRAMMAR --------------------------------------------------------------
(defconst picat-smie-grammar
  '(("eor" -10000 -10000)
    ("=>" -1200 -1200)
    ("?=>" -1200 -1200)
    ("private" nil -1150)
    ("table" nil -1150)
    ("index" nil -1150)
    ("foreach" nil -950)
    ("while" nil -950)
    ("do" nil -950)
    ("else" -950 -950)
    ("elseif" -950 -950)
    ("then" -950 -950)
    ("if" nil -950)
    ("end" -950 nil)
    (";" -940 -940)
    ("||" -940 -940)
    ("->" -935 -935)
    ("," -930 -930)
    ("&&" -930 -930)
    ("\\not" nil -900)
    ("\\once" nil -900)
    ("\\+" nil -900)
    ("\\#<=>" -850 -850)
    ("\\#=>" -840 -840)
    ("\\#\\/" -830 -830)
    ("\\#^" -820 -820)
    ("\\#/\\" -810 -810)
    ("=" -700 -700)
    ("!=" -700 -700)
    (":=" -700 -700)
    ("==" -700 -700)
    ("!==" -700 -700)
    ("\\=" -700 -700)
    ("=.." -700 -700)
    ("\\==" -700 -700)
    ("#=" -700 -700)
    ("#!=" -700 -700)
    ("#<" -700 -700)
    ("#=<" -700 -700)
    ("#<=" -700 -700)
    ("#>" -700 -700)
    ("#>=" -700 -700)
    ("#~" -700 -700)
    ("@<" -700 -700)
    ("@=<" -700 -700)
    ("@>" -700 -700)
    ("@>=" -700 -700)
    ("=:=" -700 -700)
    ("<" -700 -700)
    ("<=" -700 -700)
    ("=<" -700 -700)
    (">" -700 -700)
    (">=" -700 -700)
    ("::" -700 -700)
    ("in" -700 -700)
    ("notin" -700 -700)
    ("=.." -700 -700)
    ("++" -650 -650)
    (".." -600 -600)
    ("\\/" -550 -550)
    ("^" -540 -540)
    ("/\\" -530 -530)
    ("+" -500 -500)
    ("-" -500 -500)
    ("*" -400 -400)
    ("/" -400 -400)
    ("//" -400 -400)
    ("/<" -400 -400)
    ("/>" -400 -400)
    ("div" -400 -400)
    ("rem" -400 -400)
    ("mod" -400 -400)
    ("<<" -400 -400)
    (">>" -400 -400)
    ("~" -300 -300)
    ("**" -200 -200)
    ("." -150 -150)
    ("@" -150 -150)
    (:smie-closer-alist
     ("foreach" . "end")
     ("while" . "end")
     ("while" . "do")
     ("do" . "while")
     ("do" . "end")
     ("if" . "then")
     ("elseif" . "then")
     ("then" . "else")
     ("then" . "elseif")
     ("elseif" . "elseif")
     ("elseif" . "else")
     ("if" . "end")
     ("then" . "end")
     ("elseif" . "end")
     ("else" . "end")
     (t . "eor"))
    )
  "Precedence levels of infix operators.")


;;; FORWARD TOKEN -------------------------------------------------------------
(defun picat-smie-forward-token ()

  (forward-comment (point-max))
  (cond
   ((looking-at "[.][ \t\n]") (forward-char 1) "eor")
   (t
    (let ((str (buffer-substring-no-properties
                (point)
                (progn (cond
                        ;; ((looking-at "[;,]") (forward-char 1))
                        ((not (zerop (skip-chars-forward picat-operator-chars))))
                        ((not (zerop (skip-syntax-forward "w_'"))))
                        ((not (zerop (skip-syntax-forward ".")))))
                       (point)))))
      str))
   )) 

;;; BACKWARD TOKEN ------------------------------------------------------------
(defun picat-smie-backward-token ()

  (forward-comment (- (point-max)))
  (let ((tok (cond
              ((and (eq (char-before) ?\.)
                    (save-excursion (forward-char -1) (looking-at "[.][ \t\n]")))
               (forward-char -1)
               "eor")

              ((looking-back ")" (- (point) 1))
               (let ((bol (save-excursion (beginning-of-line) (point))))
                 (when (search-backward-regexp "\\(while\\|table\\|index\\|foreach\\)\\s-*(" bol t)
                   (let ((str (match-string-no-properties 1))) 
                     (goto-char (match-beginning 1))
                     str)))))))
    (unless tok
      (setq tok (buffer-substring-no-properties
                 (point)
                 (progn (cond
                         ;;((memq (char-before) '(?\; ?\,)) (forward-char -1))
                         ((/= 0 (skip-chars-backward picat-operator-chars)))
                         ((/= 0 (skip-syntax-backward "w_'")))
                         ((/= 0 (skip-syntax-backward "."))))
                        (point)))))
    tok))


;;; RULES ---------------------------------------------------------------------
(defun picat-smie-rules (kind token)
  ;;(message "%s %s (point: %s)" kind token (point))
  
  (pcase (cons kind token)
    (`(:before . ",")
     (smie-rule-separator kind))

    (`(:before . "=")
     (smie-rule-parent picat-indent-width))
    (`(:before . ,(or "else" "elseif" "then" "do"))
     (smie-rule-parent))
    (`(:before . ,(or "?=>" "=>"))
     '(column . 0))
    (`(:after . ,(or "?=>" "=>"))
     picat-indent-width)
    (`(:close-all . "}") t)
    ;; (`(:list-intro . ,(or "?=>" "=>"))
    ;;  picat-indent-width)
    (`(:after . "{")
     `(column . ,(+ (current-column) picat-indent-width)))
    (`(:before . "{")
     (message " parent: %s" (smie-indent--parent))
     (cond ((smie-rule-parent-p ",")
            (goto-char (nth 1 (smie-indent--parent)))
            (forward-char 1)
            (forward-comment (point-max))
            `(column . ,(current-column)))
           ;; ((smie-rule-parent-p "{")
           ;;  (backward-up-list)
           ;;  `(column . ,(+ (current-column) picat-indent-width)))
           ))
    (`(:before . ,(or "->" ";"))
     (and (smie-rule-bolp) (smie-rule-parent-p "(") (smie-rule-parent 0)))
    (`(:after . ,(or "eor" "private" "table" "index"))
     '(column . 0))
    (`(:after . ,(or "foreach" "while" "do" "->")) picat-indent-width)
    ('(:elem . basic) picat-indent-width)
    ))


(defvar picat--brackets
  '((?\{ . ?\})
    (?\( . ?\))
    (?\[ . ?\])))


;; PICAT-SMIE-INDENT -----------------------------------------------------------
(defun picat-smie-indent ()
  (picat-smie-indent-brackets))


;; PICAT-SMIE-INDENT-BRACKETS -------------------------------------------------
(defun picat-smie-indent-brackets ()

  ;;(message "picat-smie-indent-brackets")
  (let ((curchar (char-after (point)))
        (cur-tok-if-delim  (looking-at "->\\|;")) ; cur token is prolog is delim
        (prev-tok-if-delim (smie-rule-prev-p "->" ";")) ; the same for prev one
        (curpos (point))
        (indent0  0)
        (indent1  picat-indent-width)
        (indent2 (* 2 picat-indent-width)))

    (save-excursion
      (when (< 0 (nth 0 (syntax-ppss (point)))) ;; in brackets
        ;;(message "picat-smie-indent-brackets: syntax %s" (nth 0 (syntax-ppss (point))))
        (with-demoted-errors "picat-smie-indent-brackets: %s"
          (backward-up-list)
          (when (looking-at "\\s(")
            (let* ((open-bracket (char-after (point)))
                   (close-bracket (cdr (assoc open-bracket picat--brackets)))
                   (br-col (current-column)))

              ;; X = {
              ;;     y
              ;; }
              (cond
               ((and (smie-rule-hanging-p) (smie-rule-prev-p "="))
                (forward-line 0)
                (forward-comment (- br-col (current-column)))
                (+ (current-column) (if (eq curchar close-bracket) indent0 indent1)))

               ;; Prolog if expression
               ((and (eq open-bracket ?\() cur-tok-if-delim)
                (current-column))
               ((and (eq open-bracket ?\() prev-tok-if-delim)
                (+ (current-column) indent1))

               ;; closing bracket are indented as the opening bracket
               ;; first item is indented as the opening bracket plus one indent width
               (t
                (forward-char)
                (forward-comment (- curpos (point)))
                (if (= curpos (point))
                    (+ br-col (if (eq curchar close-bracket) 0 indent1))
                  (if (eq curchar close-bracket) br-col (current-column))
                  ))
               )))))
      )))


;;; MODE VARIABLES ------------------------------------------------------------
(defun picat-mode-variables ()

  (unless (boundp 'font-lock-number-face)
    (setq-local font-lock-number-face 'font-lock-number-face))
  
  (setq-local comment-start  "%"
              ;comment-add    0
              comment-column 40)
  
  (setq-local font-lock-defaults `(picat-font-lock-defaults nil nil ,picat-syntax-alist))
  
  (smie-setup picat-smie-grammar
              #'picat-smie-rules
              :forward-token #'picat-smie-forward-token
              :backward-token #'picat-smie-backward-token)

  (when picat-smie-indent-function
    (setq-local smie-indent-functions (cons picat-smie-indent-function smie-indent-functions)))
  )


;;; DEFINE MODE ---------------------------------------------------------------
;;;###autoload
(define-derived-mode picat-mode prog-mode "Picat"
  "Major mode for editing Picat code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system."
;; Commands:
;; \\{picat-mode-map}"

  (setq mode-name "Picat")
  (picat-mode-variables)
  (use-local-map picat-mode-map)
  ;; (dolist (ar prolog-align-rules) (add-to-list 'align-rules-list ar))
  ;;(add-hook 'post-self-insert-hook #'prolog-post-self-insert nil t)
  ;; `imenu' entry moved to the appropriate hook for consistency.
  ;; (when prolog-electric-dot-flag
  ;;   (setq-local electric-indent-chars
  ;;               (cons ?\. electric-indent-chars)))
  ;; (prolog-menu)
  )

;;; EASY MENU -----------------------------------------------------------------
(easy-menu-define
  picat-menu picat-mode-map
  "Commands for Picat code manipulation."
  '("Picat"
    ;; ["Beginning of clause" picat-beginning-of-clause t]
    ;; ["End of clause" picat-end-of-clause t]
    ;; ["Beginning of predicate" picat-beginning-of-predicate t]
    ;; ["End of predicate" picat-end-of-predicate t]
    ;; "---"
    ;; ["Indent line" indent-according-to-mode t]
    ;; ["Indent predicate" picat-indent-predicate t]
    ;; ["Indent buffer" picat-indent-buffer t]
    ;; ["Align region" align (use-region-p)]
    ["Compile & Load Picat File"  picat-compile-load-file]
    ["Compile Picat File"  picat-compile-file]
    ["Load Picat File"  picat-load-file]
    ;; ["Evaluate Region & Go" picat-send-region-and-go]
    ;; ["Evaluate Region" picat-send-region]
    ["--" nil]
    ["Switch to Picat" switch-to-Picat]
    ["Run Inferior Picat" run-picat]
    ))



;; run picat:
;; picat --help
;; Usage: picat [[-path Path] | [-p P] | [-s S] | [-b B] | | [-g Goal] | [-d] | [-log] | [--help] | [--version]]* PicatMainFileName A1 A2 ...
;;        P -- size for program area
;;        S -- size for global and local stacks
;;        B -- size for trail stack
;;        Path -- set PICATPATH environment variable

;;; INFERIOR PICAT MODE STUFF
;;;============================================================================
(defun picat-inferior-mode-variables ()

  (unless (boundp 'font-lock-number-face)
    (setq-local font-lock-number-face 'font-lock-number-face))
  
  (setq-local comment-start  "%")
  (setq-local comment-add    1)
  (setq-local comment-column 40)
;;  (setq-local font-lock-defaults `(picat-inferior-font-lock-defaults nil nil ,picat-syntax-alist))
  )

(defvar picat-inferior-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-l" 'picat-load-file)
    (define-key m "\C-c\C-k" 'picat-compile-load-file)
    m))

(defvar picat-buffer nil)

(define-derived-mode picat-inferior-mode comint-mode "Inferior Picat"
  "Major mode for interacting with an inferior Picat process.

The following commands are available:
\\{picat-inferior-mode-map}

A Picat process can be fired up with M-x run-picat."
  
  ;; Customize in picat-inferior-mode-hook
  (setq comint-prompt-regexp "^Picat> *")
  
  (picat-inferior-mode-variables)
  (setq mode-line-process '(":%s")))

(easy-menu-define
  picat-inferior-menu picat-inferior-mode-map
  "Commands for Picat code manipulation."
  '("Picat Shell"
    ["Compile & Load Picat File"  picat-compile-load-file]
    ["Compile Picat File"  picat-compile-file]
    ["Load Picat File"  picat-load-file]
    ))

(defun picat-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (picat-args-to-list (substring string (+ 1 where)
                                               (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (picat-args-to-list (substring string pos
                                                (length string)))))))))


(defun picat--push-arg-in-cmdargs (arg cmdargs)
  (pcase arg 
    ((pred consp) 
     (unless (or (null (cadr arg)) (member (car arg) cmdargs))
       (append arg cmdargs)))
    ((pred null) cmdargs)
    (_
     (unless (member arg cmdargs) (cons arg cmdargs))))
  )

(defun picat--push-args-in-cmdargs (args cmdargs)
  (dolist (arg (reverse args) cmdargs)
    (setq cmdargs (picat--push-arg-in-cmdargs arg cmdargs))))

;;;###autoload
(defun run-picat (&optional cmd)
  "Run an inferior Picat process, input and output via buffer `*picat*'.
If there is a process already running in `*picat*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `picat-program').
If the file `~/.picat_init' exists, it is given as initial input.
Note that this may lose due to a timing error if the Picat processor
discards input when it starts up.
Runs the hook `picat-inferior-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive
   (list (if current-prefix-arg
	     (read-string "Run Picat: " picat-program)
	   picat-program)))

  (if (null cmd) (setq cmd picat-program))
  
  (if (not (comint-check-proc "*picat*"))
      (let* ((cmdlist (picat-args-to-list cmd))
             (cmdname (car cmdlist))
             (cmdargs
              (picat--push-args-in-cmdargs `(("-path" ,picat-path)) (cdr cmdlist))))

        (set-buffer (apply 'make-comint "picat" cmdname nil cmdargs))
        (picat-inferior-mode)))
  
  (setq picat-program cmd)
  (setq picat-buffer "*picat*")
  (pop-to-buffer "*picat*"))


(defun picat-send-region (start end)
  "Send the current region to the inferior Picat process."
  (interactive "r")
  (comint-send-region (picat-proc) start end)
  (save-excursion
    (save-match-data
      (goto-char end)
      (unless (looking-back "^[ \t]*" end)
        (comint-send-string (picat-proc) "\n"))))
  )


(defun switch-to-picat (eob-p)
  "Switch to the picat process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")

  (if (or (and picat-buffer
               (get-buffer picat-buffer)
               (get-buffer-process picat-buffer))

          (run-picat))
      (pop-to-buffer picat-buffer)
    (error "No current process buffer.  See variable `picat-buffer'"))

  (when eob-p
    (push-mark)
    (goto-char (point-max))))


(defun picat-send-region-and-go (start end)
  "Send the current region to the inferior Picat process.
Then switch to the process buffer."
  (interactive "r")
  (picat-send-region start end)
  (switch-to-picat t))


(defvar picat-source-modes '(picat-mode)
  "Used to determine if a buffer contains Picat source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a picat source file by `picat-load-file'.
Used by these commands to determine defaults.")


(defvar picat-prev-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `picat-load-file' command.
Used for determining the default in the next one.")


(defun picat-load-file (file-name &optional arg)
  "Compile (if neccessary) and load a Picat file FILE-NAME into the inferior Picat process.
Switch to the Picat proccess buffer if the prefix arg is not given."

  (interactive (list (car (comint-get-source
                           "Load Picat file: " picat-prev-dir/file
                           picat-source-modes t))
                     current-prefix-arg))
  
  (comint-check-source file-name)

  (setq picat-prev-dir/file
        (cons (file-name-directory    file-name)
              (file-name-nondirectory file-name)))
  (comint-send-string (picat-proc) (concat "load(\"" file-name "\")\n"))
  (unless arg (switch-to-picat t)))


(defun picat-load-file-and-go (file-name)
  (interactive (comint-get-source
                           "Load Picat file: " picat-prev-dir/file
                           picat-source-modes t))
  (picat-load-file file-name t))


(defun picat-compile-file (file-name &optional arg)
  "Compile a Picat file FILE-NAME into the inferior Picat process.
Switch to the Picat proccess buffer if the prefix arg is not given."

  (interactive (list (car (comint-get-source
                           "Compile Picat file: " picat-prev-dir/file
                           picat-source-modes t))
                     current-prefix-arg))

  (comint-check-source file-name)
  (setq picat-prev-dir/file
        (cons (file-name-directory    file-name)
              (file-name-nondirectory file-name)))
  (comint-send-string (picat-proc) (concat "compile(\"" file-name "\")\n"))
  (unless arg (switch-to-picat t)))


(defun picat-compile-load-file (file-name &optional arg)

  (interactive (list (car (comint-get-source
                           "Compile & Load Picat file: " picat-prev-dir/file
                           picat-source-modes t))
                     current-prefix-arg))

  (picat-compile-file file-name t)
  (picat-load-file file-name arg))



(defun picat-proc ()
  "Return the current Picat process, starting one if necessary.
See variable `picat-buffer'."
  (unless (and picat-buffer
               (get-buffer picat-buffer)
               (comint-check-proc picat-buffer))
    (run-picat))
  (or (picat-get-process)
      (error "No current process.  See variable `picat-buffer'")))

(defun picat-get-process ()
  "Return the current Picat process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'picat-inferior-mode)
                          (current-buffer)
                        picat-buffer)))

;; (defun picat-interactively-start-process (&optional cmd)
;;   "Start an inferior Picat process.  Return the process started.
;; Since this command is run implicitly, always ask the user for the
;; command to run."
;;   (save-window-excursion
;;     (run-picat (read-string "Run Picat: " picat-program))))

;;; Do the user's customization...

(run-hooks 'picat-load-hook)

(provide 'picat-mode)
(provide 'picat-inferior-mode)
