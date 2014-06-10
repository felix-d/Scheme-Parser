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

(define (get-postscript sym)
  (cond ((eq? '+ sym)'add)
        ((eq? '- sym)'sub)
        ((eq? '* sym)'mul)
        ((eq? '/ sym)'div)))

(define (get-func sym)
  (cond ((eq? '+ sym)(lambda(x y)(+ x y)))
        ((eq? '- sym)(lambda(x y)(- x y)))
        ((eq? '/ sym)(lambda(x y)(/ x y)))
        ((eq? '* sym)(lambda(x y)(* x y)))))

(define (get-precedance sym)
  (cond ((eq? '+ sym) 0)
        ((eq? '- sym) 0)
        ((eq? '/ sym) 1)
        ((eq? '* sym) 1)))

(define (get-dependance sym)
  (cond ((eq? '+ sym) 0)
        ((eq? '- sym) 1)
        ((eq? '/ sym) 1)
        ((eq? '* sym) 0)))

;;List of operators
(define operators '(+ - / *))

;;Display errors
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

;;'(('data. data)'('('lchild. leftchild)'('rchild . rightchild))
(define (make-node data children)
  (if (null? children)
      (list (cons 'data data)'())
      (list (cons 'data data)
            (list (cons 'lchild (car children))
                  (cons 'rchild (cadr children))))))

;;Get children
(define (get-children node)
  (if(not(null? node))(cdr node)))

;;Get data of a node
(define (get-data node)
  (let ((x (assoc 'data node)))
    (if (eq? #f x) '() (cdr x))))

;;Get left child of node
;;If node is empty, returns empty.
;;If node is a leaf and has no children, return empty
(define (get-lchild node)
  (if (null? node) '()
  (let ((x (assoc 'lchild (cadr node))))
    (if (eq? x #f) '() (cdr x)))))

;;Get right child of node
(define (get-rchild node)
  (if (null? node) '()
  (let ((x (assoc 'rchild(cadr node))))
    (if (eq? x #f) '() (cdr x)))))

;;Is node a leaf?
(define (leaf? node)
  (null? (car(get-children node))))

;;Get last two elements of list as a list
;;Used to get last two elements on the stack
(define (get-last-two liste)
  (let ((l (reverse liste)))
    (list (cadr l) (car l))))

(define (remove-last-two liste)
  (let ((l (reverse liste)))
        (reverse(cddr l))))

;;MAIN PARSING FUNCTION
(define (parse tree)
  (define (parse-helper chaine stack tree)
    (cond
     ((and (null? chaine) (not(= 1 (length stack)))) 'ERROR_syntax_error)
     (else (if (null? chaine)
               (car tree)
               (let((c (car chaine)))
                 (cond
                  ((null? c) (car tree))
                  ((number? c)
                   (parse-helper (cdr chaine)
                                 (append stack (list(make-node c '())))
                                 tree))
                  ((symbol? c)
                   (cond
                    ((< (length stack) 2) 'ERROR_syntax_error)
                    (else (parse-helper (cdr chaine)
                                        (append (remove-last-two stack)
                                                (list(make-node c (get-last-two stack))))
                                        (list(make-node c (get-last-two stack)))))))))))))
  (parse-helper tree '() '()))

(define (print-tree tree)
  (define (print-tree-help tree n)
    (print-spaces n)
    (display (get-data tree))
    (newline)
    (cond ((not(leaf? tree))
           (print-tree-help (get-rchild tree) (+ n 1))
           (print-tree-help (get-lchild tree) (+ n 1)))))
  (print-tree-help tree 0))

(define (print-spaces n)
  (cond ((= n 0))
        (else (display "  ")
              (print-spaces (- n 1)))))

(define (display-scheme tree)
  (define (display-scheme-helper tree str)
    (cond((not(leaf? tree))
          (string-append str " (" (symbol->string(get-data tree))
                         (display-scheme-helper (get-lchild tree) str)
                         (display-scheme-helper (get-rchild tree) str) ")"))
         (else
          (string-append str " " (number->string(get-data tree))))))
  (string->list(display-scheme-helper tree "")))

(define (display-postscript tree)
  (define (display-postscript-helper tree str)
    (cond ((not(leaf? tree))
           (string-append str
                          (display-postscript-helper (get-lchild tree) str)
                          (display-postscript-helper (get-rchild tree) str)
                          " "
                          (symbol->string(get-postscript(get-data tree)))))
          (else
           (string-append str " " (number->string(get-data tree))))))
  (string->list(display-postscript-helper tree "")))

(define (display-C tree)
  (define (display-C-helper tree str)
    (if (not(leaf? tree))
        (string-append
         str
         (cond ((and
                 (not(leaf? (get-lchild tree)))
                 (> (get-precedance (get-data tree))
                    (get-precedance(get-data (get-lchild tree)))))
                (string-append "("
                               (display-C-helper
                                (get-lchild tree)
                                str)
                               ")"))
               (else
                (display-C-helper (get-lchild tree) str)))

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
                (string-append "(" (display-C-helper (get-rchild
                                                      tree) str)
                               ")"))
               (else (display-C-helper (get-rchild tree) str))))
        (string-append str (number->string(get-data tree)))))
  (string->list(display-C-helper tree "")))

(define (preprocess input)
  (define (preprocess-helper input numbers output)
    (cond ((and (not(null? numbers))(null? input)) 'ERROR_syntax_error)
          ((and (null? input)(null? output)) 'ERROR_empty_expression)
          ((and (null? input)(null? numbers)(not(null? output))) output)
          (else
           (let ((digit (string->number (list->string(list(car input)))))
                 (num (string->number(list->string numbers)))
                 (sym (string->symbol(list->string(list(car input)))))
                 (numbers? (not(null? numbers))))
             (cond ((number? digit)
                    (preprocess-helper (cdr input)
                                (append numbers (list(car input)))
                                output))
                   ((eq? (car input) #\space)
                    (preprocess-helper (cdr input)
                                '()
                                (if numbers?
                                    (append output (list num))
                                    output)))
                   ((member sym operators)
                    (preprocess-helper (cdr input)
                                '()
                                (if numbers?
                                    (append output (list num) (list sym))
                                    (append output (list sym)))))
                   (else 'ERROR_unknown_char))))))
  (preprocess-helper input '() '()))

(define (precise-division n d)
  (define (helper n d l ndl)
    (cond
     ((= 0 (remainder n d)) (print-f-number (append l (list(quotient n d))) -1))
     (else (if (= 0 (quotient n d))
               (let ((x (* 10 n)))
                 (let ((pe (pair-exists? ndl x d)))
                   (if pe
                       (print-f-number l pe)
                       (helper x
                               d
                               (append l '(0))
                               (append ndl (list (cons n d)))))))
               (let ((x (* (- n (* d (quotient n d))) 10)))
                 (let ((pe (pair-exists? ndl x d)))
                   (if pe
                       (print-f-number l pe)
                       (helper x
                               d
                               (append l (list (quotient n d)))
                               (append ndl (list(cons n d)))))))))))
  (helper n d '() '()))

(define (print-f-number l pe)
  (define (print-f-number-helper l str n)
    (if (null? l)
        (if(= -1 pe) str (string-append str "]"))
        (cond
         ((= n 0)
               (print-f-number-helper
                (cdr l)
                (string-append str (number->string(car l)) ".")
                1))
              ((= n pe)
               (print-f-number-helper
                (cdr l)
                (string-append str "[" (number->string(car l)))
                (+ n 1)))
              (else
               (print-f-number-helper
                (cdr l)
                (string-append str (number->string(car l)))
                (+ n 1))))))
  (print-f-number-helper l "" 0))

(define (pairs-equal? p1 p2)
  (if (and (= (car p1) (car p2)) (= (cdr p1)(cdr p2))) #t #f))

(define (pair-exists? liste a b)
  (if (null? liste) #f
      (if(assoc a liste)
         (if (and(= (car(assoc a liste)) a) (= (cdr(assoc a liste)) b))
             (list-index (cons a b) liste 0)
             (pair-exists? (cdr liste) a b))
         #f)))

(define (list-index e l n)
  (if (null? l) -1
      (if (pairs-equal? (car l) e) n
          (list-index e (cdr l) (+ 1 n)))))

;;Get value of current tree
(define (get-value tree)
  (define (helper t)
    (cond ((number? (get-data t)) (get-data t))
          (else
           (let ((v (helper (get-rchild t))))

             (cond ((= v 0) 'ERROR_division_by_zero)
                   (else
                    ((get-func (get-data t))
                    (helper (get-lchild t))
                    v)))))))
  (let ((result (helper tree)))
    (cond ((integer? result) (number->string result))
          ((symbol? result) result)
          (else(precise-division (numerator result)
                                 (denominator result))))))



;;TRAITER EXPRESSION
(define traiter
  (lambda (expr)
    (let((e (preprocess expr)))
      (if (symbol? e) ; preprocessing error
          (display-error e expr)
          (let((ee (parse e)))
            (cond ((symbol? ee) ; parsing error
                (display-error ee expr))
                  (else
                   (let ((eee (get-value ee))) (if(symbol? eee) (display-error
                                                                 eee expr)
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

;;TESTING
;;(print-tree (parse (preprocess (string->list "1 2 + 5 /"))))
;;(parse '())
;(display-scheme tree1)
;;(print (get-data tree1))
;(print-tree tree1)
(define tree2 (parse '(1 10 /)))
(get-value tree2)

;;(leaf? tree2)

;;(get-data(get-rchild tree2))
;;(get-rchild tree2)
;;(get-data tree2)
;;(parse '())
;;(leaf? tree2)
(integer? 3/4)
