
;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant l'expression lue et retourne une liste de caractères qui
;;; sera imprimée comme résultat de l'expression entrée.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "go" qui se charge de cela.


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
  (define (helper chaine stack tree)
    (cond
     ((and (null? chaine) (not(= 1 (length stack)))) 'ERROR_syntax_error)
     (else (if (null? chaine)
               (car tree)
               (let((c (car chaine)))
                 (cond
                  ((null? c) (car tree))
                  ((number? c)
                   (helper (cdr chaine)
                                 (append stack (list(make-node c '())))
                                 tree))
                  ((symbol? c)
                   (cond
                    ((< (length stack) 2) 'ERROR_syntax_error)
                    (else (helper (cdr chaine)
                                        (append (remove-last-two stack)
                                                (list(make-node c (get-last-two stack))))
                                        (list(make-node c (get-last-two stack)))))))))))))
  (helper liste '() '()))


;;;----------------------------------------------------------------------------
;;; Pretty-print a tree. For debugging purposes.
;;;----------------------------------------------------------------------------

(define (print-tree tree)
  (define (print-spaces n)
    (cond ((= n 0))
          (else (display "  ")
                (print-spaces (- n 1)))))
  (define (helper tree n)
    (print-spaces n)
    (display (get-data tree))
    (newline)
    (cond ((not(leaf? tree))
           (helper (get-rchild tree) (+ n 1))
           (helper (get-lchild tree) (+ n 1)))))
  (helper tree 0))


;;;----------------------------------------------------------------------------
;;; Returns Scheme syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-scheme tree)
  (define (helper tree str)
    (cond((not(leaf? tree))
          (string-append str " (" (symbol->string(get-data tree))
                         (helper (get-lchild tree) str)
                         (helper (get-rchild tree) str) ")"))
         (else
          (string-append str " " (number->string(get-data tree))))))
  (string->list(helper tree "")))


;;;----------------------------------------------------------------------------
;;; Returns Postscript syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-postscript tree)
  (define (helper tree str)
    (cond ((not(leaf? tree))
           (string-append str
                          (helper (get-lchild tree) str)
                          (helper (get-rchild tree) str)
                          " "
                          (symbol->string(get-postscript(get-data tree)))))
          (else
           (string-append str " " (number->string(get-data tree))))))
  (string->list(helper tree "")))


;;;----------------------------------------------------------------------------
;;; Returns C syntax for the given tree.
;;;----------------------------------------------------------------------------

(define (display-C tree)
  (define (helper tree str)
    (if (not(leaf? tree))
        (string-append
         str
         (cond ((and
                 (not(leaf? (get-lchild tree)))
                 (> (get-precedance (get-data tree))
                    (get-precedance(get-data (get-lchild tree)))))
                (string-append "(" (helper (get-lchild tree) str) ")"))
               (else (helper (get-lchild tree) str)))
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
                (string-append "(" (helper (get-rchild
                                                      tree) str)
                               ")"))
               (else (helper (get-rchild tree) str))))
        (string-append str (number->string(get-data tree)))))
  (string->list(helper tree "")))


;;;----------------------------------------------------------------------------
;;; Transforms a list of characters to a parsable list.
;;;----------------------------------------------------------------------------

(define (preprocess input)
  (define (helper input numbers output)
    (cond ((and (not(null? numbers))(null? input)) 'ERROR_syntax_error)
          ((and (null? input)(null? output)) 'ERROR_empty_expression)
          ((and (null? input)(null? numbers)(not(null? output))) output)
          (else
           (let ((digit (string->number (list->string(list(car input)))))
                 (num (string->number(list->string numbers)))
                 (sym (string->symbol(list->string(list(car input)))))
                 (numbers? (not(null? numbers))))
             (cond ((number? digit)
                    (helper (cdr input)
                                (append numbers (list(car input)))
                                output))
                   ((eq? (car input) #\space)
                    (helper (cdr input)
                                '()
                                (if numbers?
                                    (append output (list num))
                                    output)))
                   ((member sym operators)
                    (helper (cdr input)
                                '()
                                (if numbers?
                                    (append output (list num) (list sym))
                                    (append output (list sym)))))
                   (else 'ERROR_unknown_char))))))
  (helper input '() '()))


;;;----------------------------------------------------------------------------
;;; Takes a numerator and denominator and returns a formatted representation
;;; of the result. l is the list of numbers contained in the result of the division
;;; algorithm and ndl is the list of pairs of divided numbers seen during the
;;; algorithm. The algorithm stop if it encounters a pair in ndl.
;;;----------------------------------------------------------------------------

(define (precise-division n d)
  (define (helper n l ul)
     (cond
      ((= 0 (remainder n d)) (print-f-number (append l (list(quotient n d))) -1))
      (else (if (= 0 (quotient n d))
                (let ((x (* 10 n)))
                  (let ((pe (list-index ul x)))
                    (if pe
                        (print-f-number l pe)
                        (helper x
                                (append l '(0))
                                (append ul (list n))))))
                (let ((x (* (- n (* d (quotient n d))) 10)))
                  (let ((pe (list-index ul n)))
                    (if pe
                        (print-f-number l pe)
                        (helper x
                                (append l (list (quotient n d)))
                                (if (not(null? l))
                                    (append ul (list n))
                                    ;to avoid brackets on the first number
                                    (append ul (list 0)))))))))))
  (if (< n 0)
      (string-append "-" (helper (abs n) '() '()))
      (helper n '() '())))


;;;----------------------------------------------------------------------------
;;; Takes the list of numbers to be formatted, the index of the first
;;; occurence of the repeated character, and returns the formatted result as a
;;; string. If pe is -1, no square brackets are added because there is no
;;; repeated decimals.
;;;----------------------------------------------------------------------------

(define (print-f-number l pe)
  (define (helper l str n c)
    (if (null? l) ;If end of input?
        (if(= -1 pe) str (string-append str "]"))
        (cond ((= n 0)
               (helper
                (cdr l)
                (string-append str (number->string(car l)) ".")
                (+ n 1)
                (car l)))
              ((= n pe) ;if current iteration is first repeated number
               (helper (cdr l)
                        (string-append str "[" (number->string(car l)))
                        (+ n 1) '()))
              (else (helper (cdr l)
                             (string-append str (number->string(car l)))
                             (+ n 1) '())))))
  (helper l "" 0 '()))


;;;----------------------------------------------------------------------------
;;; Returns the value of the given tree.
;;;----------------------------------------------------------------------------

(define (get-value tree)
  (define (helper t)
    (cond ((number? (get-data t)) (get-data t))
          (else
           (let ((v (helper (get-rchild t))))

             (cond ((and(= v 0) (eq? '/ (get-data t))) 'ERROR_division_by_zero)
                   (else
                    ((get-func (get-data t))
                    (helper (get-lchild t))
                    v)))))))
  (let ((result (helper tree)))
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
