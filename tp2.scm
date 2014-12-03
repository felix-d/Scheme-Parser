;;; SCHEME PARSER
;;; Authors: Felix Descoteaux & Pascal Champagne


;;;----------------------------------------------------------------------------
;;; If e is in the list, get its index, else return false.
;;;----------------------------------------------------------------------------

(define list-index
  (lambda (lst e)
    (if (null? lst)
        #f
        (if (= (car lst) e)
            0
            (if (eq? (list-index (cdr lst) e) #f)
                #f
                (+ 1 (list-index (cdr lst) e)))))))


;;;----------------------------------------------------------------------------
;;; Transforms a list of digits to a number
;;;----------------------------------------------------------------------------

(define (list->number i)
  (string->number(list->string i)))


;;;----------------------------------------------------------------------------
;;; Transforms a list containing a character to a symbol
;;;----------------------------------------------------------------------------

(define (list->symbol i)
  (string->symbol(list->string i)))


;;;----------------------------------------------------------------------------
;;; Get postscript symbol matching the operator.
;;;----------------------------------------------------------------------------

(define (get-postscript sym)
  (cond ((eq? '+ sym)'add)
        ((eq? '- sym)'sub)
        ((eq? '* sym)'mul)
        ((eq? '/ sym)'div)))


;;;----------------------------------------------------------------------------
;;; Get lambda matching the operator.
;;;----------------------------------------------------------------------------

(define (get-func sym)
  (cond ((eq? '+ sym)(lambda(x y)(+ x y)))
        ((eq? '- sym)(lambda(x y)(- x y)))
        ((eq? '/ sym)(lambda(x y)(/ x y)))
        ((eq? '* sym)(lambda(x y)(* x y)))))


;;;----------------------------------------------------------------------------
;;; Get precedance level of the operator.
;;;----------------------------------------------------------------------------

(define (get-precedance sym)
  (cond ((eq? '+ sym) 0)
        ((eq? '- sym) 0)
        ((eq? '/ sym) 1)
        ((eq? '* sym) 1)))


;;;----------------------------------------------------------------------------
;;; Get dependance level of the operator.
;;;----------------------------------------------------------------------------

(define (get-dependance sym)
  (cond ((eq? '+ sym) 0)
        ((eq? '- sym) 1)
        ((eq? '/ sym) 1)
        ((eq? '* sym) 0)))


;;;----------------------------------------------------------------------------
;;; Simple list of operators for matching purposes.
;;;----------------------------------------------------------------------------

(define operators '(+ - / *))


;;;----------------------------------------------------------------------------
;;; Get error matching error symbol. Some include faulting expression.
;;;----------------------------------------------------------------------------

(define (display-error e expr)
  (cond
   ((eq? e 'ERROR_empty_expression)
    (string->list "EXPRESSION VIDE\n\n"))
   ((eq? e 'ERROR_syntax_error)
    (string->list (string-append "ERREUR DE SYNTAXE DANS L'EXPRESSION: "
                                 (list->string expr) "\n\n")))
   ((eq? e 'ERROR_unknown_char)
    (string->list (string-append "CARACTERE INCONNU DANS L'EXPRESSION: "
                                 (list->string expr) "\n\n")))
   ((eq? e 'ERROR_division_by_zero)
    (string->list (string-append "ERREUR DIVISION PAR ZERO DANS L'EXPRESSION: "
                                 (list->string expr) "\n\n")))))


;;;----------------------------------------------------------------------------
;;; Creates a node from data and its children.
;;; The form of the node is
;;;'(('data . data) '(('lchild . lchild) ('rchild .rchild)))
;;;----------------------------------------------------------------------------

(define (make-node data children)
  (if (null? children)
      (list (cons 'data data)'())
      (list (cons 'data data)
            (list (cons 'lchild (car children))
                  (cons 'rchild (cadr children))))))


;;;----------------------------------------------------------------------------
;;; Get children of a node.
;;;----------------------------------------------------------------------------

(define (get-children node)
  (if(not(null? node))(cdr node)))


;;;----------------------------------------------------------------------------
;;; Get data of a node.
;;;----------------------------------------------------------------------------

(define (get-data node)
  (let ((x (assoc 'data node)))
    (if (eq? #f x) '() (cdr x))))



;;;----------------------------------------------------------------------------
;;; Get left child of a node. If child doesn't exist, return '().
;;;----------------------------------------------------------------------------

(define (get-lchild node)
  (if (null? node) '()
  (let ((x (assoc 'lchild (cadr node))))
    (if (eq? x #f) '() (cdr x)))))


;;;----------------------------------------------------------------------------
;;; Get right child of a node. If child doesn't exist, return '().
;;;----------------------------------------------------------------------------

(define (get-rchild node)
  (if (null? node) '()
  (let ((x (assoc 'rchild(cadr node))))
    (if (eq? x #f) '() (cdr x)))))


;;;----------------------------------------------------------------------------
;;; Check if a node is a leaf.
;;;----------------------------------------------------------------------------

(define (leaf? node)
  (null? (car(get-children node))))


;;;----------------------------------------------------------------------------
;;; Get last two elements of list as a list.
;;;----------------------------------------------------------------------------

(define (get-last-two liste)
  (let ((l (reverse liste)))
    (list (cadr l) (car l))))


;;;----------------------------------------------------------------------------
;;; Remove last two elements of list and returns it.
;;;----------------------------------------------------------------------------

(define (remove-last-two liste)
  (let ((l (reverse liste)))
        (reverse(cddr l))))


;;;----------------------------------------------------------------------------
;;; Main parsing function. Takes a list of numbers and operators an returns
;;; the root of parsed tree.
;;;----------------------------------------------------------------------------

(define (parse liste)
  (define (parse-h chaine stack tree)
    (cond
     ((and (null? chaine) (not(= 1 (length stack)))) 'ERROR_syntax_error)
     (else (if (null? chaine)
               (car tree)
               (let((c (car chaine)))
                 (cond
                  ((null? c) (car tree))
                  ((number? c)
                   (parse-h (cdr chaine)
                                 (append stack (list(make-node c '())))
                                 tree))
                  ((symbol? c)
                   (cond
                    ((< (length stack) 2) 'ERROR_syntax_error)
                    (else (parse-h (cdr chaine)
                                        (append (remove-last-two stack)
                                                (list(make-node c (get-last-two stack))))
                                        (list(make-node c (get-last-two
							   stack)))))))))))))
  (if (= 1 (length liste))
      (if(number? (car liste)) ;If there's only a number
	 (make-node (car liste) '()) ;If there's only a character
	 'ERROR_syntax_error)
	 (parse-h liste '() '()))) ;Else


;;;----------------------------------------------------------------------------
;;; Pretty-print a tree. For debugging purposes.
;;;----------------------------------------------------------------------------

(define (print-tree tree)
  (define (print-spaces n)
    (cond ((= n 0))
          (else (display "  ")
                (print-spaces (- n 1)))))
  (define (print-tree-h tree n)
    (print-spaces n)
    (display (get-data tree))
    (newline)
    (cond ((not(leaf? tree))
           (print-tree-h (get-rchild tree) (+ n 1))
           (print-tree-h (get-lchild tree) (+ n 1)))))
  (print-tree-h tree 0))


;;;----------------------------------------------------------------------------
;;; Returns Scheme syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-scheme tree)
  (define (display-scheme-h tree str)
    (cond((not(leaf? tree))
          (string-append str " (" (symbol->string(get-data tree))
                         (display-scheme-h (get-lchild tree) str)
                         (display-scheme-h (get-rchild tree) str) ")"))
         (else
          (string-append str " " (number->string(get-data tree))))))
  (string->list(display-scheme-h tree "")))


;;;----------------------------------------------------------------------------
;;; Returns Postscript syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-postscript tree)
  (define (display-postscript-h tree str)
    (cond ((not(leaf? tree))
           (string-append str
                          (display-postscript-h (get-lchild tree) str)
                          (display-postscript-h (get-rchild tree) str)
                          " "
                          (symbol->string(get-postscript(get-data tree)))))
          (else
           (string-append str " " (number->string(get-data tree))))))
  (string->list(display-postscript-h tree "")))


;;;----------------------------------------------------------------------------
;;; Returns C syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-C tree)
  (define (display-C-h tree str)
    (if (not(leaf? tree))
        (string-append
         str
         (cond ((and
                 (not(leaf? (get-lchild tree)))
                 (> (get-precedance (get-data tree))
                    (get-precedance(get-data (get-lchild tree)))))
                (string-append "(" (display-C-h (get-lchild tree) str) ")"))
               (else (display-C-h (get-lchild tree) str)))
         (symbol->string(get-data tree))
         (cond ((or
                 (and
                  (not(leaf? (get-rchild tree)))
                  (> (get-precedance (get-data tree))
                     (get-precedance (get-data (get-rchild tree)))))
                 (and
                  (not(leaf? (get-rchild tree)))
                  (eq? (get-precedance(get-data tree))
                       (get-precedance(get-data(get-rchild tree))))
                  (eq? (get-dependance (get-data tree)) 1)))
                (string-append "(" (display-C-h (get-rchild
                                                      tree) str)
                               ")"))
               (else (display-C-h (get-rchild tree) str))))
        (string-append str (number->string(get-data tree)))))
  (string->list(display-C-h tree "")))


;;;----------------------------------------------------------------------------
;;; Transforms a list of characters to a parsable list.
;;;----------------------------------------------------------------------------

(define (preprocess input)
  (define (preprocess-h input numbers output)
    (cond
					;If there are still numbers
					;in the stack but no other character to process
     ((and (not(null? numbers))(null? input)(not(null? output)))
      'ERROR_syntax_error)
					;If the expression is empty
     ((and (null? input)(null? output)(null? numbers))
      'ERROR_empty_expression)
					;If there is only a number
     ((and (null? input)(null? output)) (list(list->number numbers)))
     ((and (null? input)(null? numbers)(not(null? output))) output)
     (else
      (let ((digit (list->number(list(car input))))
	    (num (list->number numbers))
	    (sym (list->symbol(list(car input))))
	    (numbers? (not(null? numbers))))
					;Input is either a digit, op or space
					;If its a space or op, check for digit stacked in numbers
					;and add number to list
	(cond ((number? digit)
	       (preprocess-h (cdr input)
		       (append numbers (list(car input)))
		       output))
	      ((eq? (car input) #\space)
	       (preprocess-h (cdr input) '()
		       (if numbers?
			   (append output (list num))
			   output)))
	      ((member sym operators)
	       (preprocess-h (cdr input) '()
		       (if numbers?
			   (append output (list num) (list sym))
			   (append output (list sym)))))
                   (else 'ERROR_unknown_char))))))
  (preprocess-h input '() '()))

;;;----------------------------------------------------------------------------
;;; Takes a numerator and denominator and returns a formatted representation
;;; of the result. l is the list of numbers contained in the result of the division
;;; algorithm and ndl is the list of divided numbers seen during the
;;; algorithm. The algorithm stop if it encounters a number in ul.
;;;----------------------------------------------------------------------------

(define (precise-division n d)
  (define (precise-divison-h n l ul)
     (cond
      ((= 0 (remainder n d)) (print-f-number (append l (list(quotient n d))) -1))
      (else (if (= 0 (quotient n d))
		(let ((pe (list-index ul n)))
		  (if pe
		      (print-f-number l pe)
		      (precise-divison-h (* 10 n)
			      (append l '(0))
			      (append ul (list n)))))
		(let ((pe (list-index ul n)))
		  (if pe
		      (print-f-number l pe)
		      (precise-divison-h (* (- n (* d (quotient n d))) 10)
			      (append l (list (quotient n d)))
			      (if (not(null? l))
				  (append ul (list n))
                                    ;to avoid brackets on the first number
                                    (append ul (list 0))))))))))
  ;For negative numbers
  (if (< n 0)
      (string-append "-" (precise-divison-h (abs n) '() '()))
      (precise-divison-h n '() '())))


;;;----------------------------------------------------------------------------
;;; Takes the list of numbers to be formatted, the index of the first
;;; occurence of the repeated character, and returns the formatted result as a
;;; string. If pe is -1, no square brackets are added because there is no
;;; repeated decimals.
;;;----------------------------------------------------------------------------

(define (print-f-number l pe)
  (define (print-f-number-h l str n)
    (if (null? l) ;If end of input?
        (if(= -1 pe) str (string-append str "]"))
        (cond ((= n 0)
               (print-f-number-h
                (cdr l)
                (string-append str (number->string(car l)) ".")
                (+ n 1)))
              ((= n pe) ;if current iteration is first repeated number
               (print-f-number-h (cdr l)
                        (string-append str "[" (number->string(car l)))
                        (+ n 1)))
              (else (print-f-number-h (cdr l)
                             (string-append str (number->string(car l)))
                             (+ n 1))))))
  (print-f-number-h l "" 0))


;;;----------------------------------------------------------------------------
;;; Returns the value of the given tree.
;;;----------------------------------------------------------------------------

(define (get-value tree)
  (define (get-value-h t)
    (cond ((number? (get-data t)) (get-data t))
          (else
           (let ((v (get-value-h (get-rchild t))))

             (cond ((and(= v 0) (eq? '/ (get-data t))) 'ERROR_division_by_zero)
                   (else
                    ((get-func (get-data t))
                    (get-value-h (get-lchild t))
                    v)))))))
  (let ((result (get-value-h tree)))
    (cond ((integer? result) (number->string result))
          ((symbol? result) result)
          (else(precise-division (numerator result)
                                 (denominator result))))))



;;;----------------------------------------------------------------------------
;;; Treats the given list of characters. If there is an error during
;;; preprocessing, parsing or while computing the value, prints it. Else,
;;; print the different syntaxes.
;;;----------------------------------------------------------------------------

(define traiter
  (lambda (expr)
    (let((e (preprocess expr)))
      (if (symbol? e) ; preprocessing error
          (display-error e expr)
          (let((ee (parse e)))
            (cond ((symbol? ee) ; parsing error
                (display-error ee expr))
                  (else
                   (let ((eee (get-value ee)))
                     (if(symbol? eee) ; Evaluation error
                        (display-error eee expr)
                        (append (string->list "    Scheme:")
                                (display-scheme ee)
                                '(#\newline)
                                (string->list "         C: ")
                                (display-C ee)
                                '(#\newline)
                                (string->list "Postscript:")
                                (display-postscript ee)
                                '(#\newline)
                                (string->list "     Value: ")
                                (string->list(get-value ee))
                                '(#\newline)
                                '(#\newline)))))))))))

;;;----------------------------------------------------------------------------
;;; Ne pas modifier cette section.

(define go
  (lambda ()
    (print "EXPRESSION? ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (begin
            (for-each write-char (traiter-ligne ligne))
            (go))))))

(define traiter-ligne
  (lambda (ligne)
    (traiter (string->list ligne))))

(go)

;;;----------------------------------------------------------------------------
