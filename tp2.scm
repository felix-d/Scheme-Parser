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

;;Get value of current tree
(define (get-value tree)
  (cond ((number? (get-data tree)) (get-data tree))
        (else ((get-func (get-data tree))
               (get-value (get-lchild tree))
               (let ((v (get-value (get-rchild tree)))) (if(= v 0)
                                                           'ERROR_division_by_zero
                                                           v))))))
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
                                (string->list(number->string (get-value ee)))
                                '(#\newline)
                                '(#\newline)))))))))

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
;(define tree2 (parse '(1 3 +)))
;;(leaf? tree2)

;;(get-data(get-rchild tree2))
;;(get-rchild tree2)
;;(get-data tree2)
;;(parse '())
;;(leaf? tree2)
