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
    (string->list "Expression vide\n"))
   ((eq? e 'ERROR_syntax_error)
    (string->list "Erreur de syntaxe\n"))))

;;Return a node in the form of a list
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
  (parse-helper tree '() '()))
(define (parse-helper chaine stack tree)
  (cond
   ((and (null? chaine) (null? tree) (null? stack)) 'ERROR_empty_expression)
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
                  ((not(member c operators)) 'ERROR_unknown_char)
                  ((< (length stack) 2) 'ERROR_syntax_error)
                                    (else (parse-helper (cdr chaine)
                               (append (remove-last-two stack)
                                       (list(make-node c (get-last-two stack))))
                               (list(make-node c (get-last-two stack)))))))))))))

(define (print-tree tree)
  (print-tree-help tree 0))

(define (print-tree-help tree n)
  (cond ((leaf? tree))
        (else
         (print-spaces n)
         (display (get-data tree)) (newline)
         (print-tree-help (get-rchild tree) (+ n 1))
         (print-tree-help (get-lchild tree) (+ n 1)))))

(define (print-spaces n)
  (cond ((= n 0))
        (else (display "  ")
              (print-spaces (- n 1)))))

(define (display-scheme tree)
  (if(not(leaf? tree))(display "("))
  (display(get-data tree))
  (if(not(leaf? tree))
     (display-scheme (get-lchild tree)))
  (if(not(leaf? tree))
     (display-scheme (get-rchild tree)))
  (if(not(leaf? tree))(display ")")))


(define (inorder-traversal tree)
  (if(not(leaf? tree)) (inorder-traversal (get-lchild tree)))
  (display (get-data tree))
  (if(not(leaf? tree)) (inorder-traversal (get-rchild tree))))

(define (preorder-traversal tree)
  (display(get-data tree))
 (if(not(leaf? tree)) (preorder-traversal (get-lchild tree)))
 (if(not(leaf? tree)) (preorder-traversal (get-rchild tree))))



(define (postorder-traversal tree)
  (if(not(leaf? lc))
                                  (postorder-traversal (get-lchild tree))))
  (let((rc (get-rchild tree))) (if(not(leaf? rc))
                                  (postorder-traversal (get-rchild tree))))
  (display (get-data tree)))

;;TRAITER EXPRESSION
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

;(go)

;;;----------------------------------------------------------------------------

;;TESTING
(parse '(+ + + +))
(define tree (parse '(1 3 4 + 5 6 4 - * + /  2 1 - +)))
(display-scheme tree)
(print (get-data tree))
(print-tree tree)
(define tree (parse '(1 3 +)))
(leaf? tree)

(get-data(get-rchild tree))
(get-rchild tree)
(get-data tree)
(parse '())
(leaf? tree)
