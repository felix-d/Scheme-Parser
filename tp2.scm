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

;;Operations
(define (addition x y) (+ x y))
(define (soustraction x y) (- x y))
(define (division x y) (/ x y))
(define (multiplication x y) (* x y))

;;Symbol precedance dependance function
(define operator+ (list + #f #f addition))
(define operator- (list - #f #t soustraction))
(define operator/ (list / #t #t division))
(define operator* (list * #t #f multiplication))

;;List of operators
(define operators '(+ - / *))

;;Display errors
(define (display-error e)
  (cond
   ((eq? e 'ERROR_empty_expression)
    (string->list "Expression vide\n"))))

;;Return a node in the form of a list
;;'(('data. data)'('('lchild. leftchild)'('rchild . rightchild))
(define (make-node data children)
  (if (null? children)
      (list (cons 'data data)'())
      (list (cons 'data data)(list (cons 'lchild (car children))(cons 'rchild (cadr children))))))

;;Get children
(define (get-children node)
  (cdr node))

;;Get data of a node
(define (get-data node)
  (cdr (assoc 'data node)))

;;Get left child of node
(define (get-lchild node)
  (cdr(assoc 'lchild (cadr node))))

;;Get right child of node
(define (get-rchild node)
 (cdr(assoc 'rchild (cadr node))))

;;Is node a leaf?
(define (leaf? node)
  (null? (children node)))

;;Get last two elements of list as a list
;;Used to get last two elements on the stack
(define (get-last-two liste)
  (let ((l (reverse liste)))
    (list (cadr l) (car l))))

;;MAIN PARSING FUNCTION
(define (parse chaine stack tree)
  (if (and (null? chaine) (null? tree)) 'ERROR_empty_expression
      (if (null? chaine) (car tree)
          (let((c (car chaine)))
            (cond
             ((null? c) (car tree))
             ((number? c)
              (parse (cdr chaine)(append stack (list(make-node c '()))) tree))
             ((symbol? c)
              (parse (cdr chaine)(append stack (list(make-node c (get-last-two stack))))
                     (list(make-node c (get-last-two stack))))))))))

;;TRAITER EXPRESSION - initialement fourni par feely
(define traiter
  (lambda (expr)
    (let((e (parse expr '() '())))
    (if (symbol? e) (display-error e) '()))))

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
(parse '(1 3 4 + 5 6 - * /) '() '())
(define tree (parse '(1 3 4 + 5 6 - * /) '() '()))
(get-data(get-rchild(get-rchild tree)))
(get-rchild tree)
(get-data tree)
(parse '() '() '())
