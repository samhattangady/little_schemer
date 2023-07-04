;; things.lisp is a file containing all the functions that the
;; book little_schemer defines over the course of the chapters
;; since the book builds on previous things, but the repl dies
;; we want a single source for all the things.

;; returns true if number is zero
(defun zero? (a)
  (eq 0 a)
)

;; returns true if a is an atom
(defun atom? (a)
  (not (listp a))
)

;; returns true if l is a list of atoms
(defun lat? (l)
  (cond
    ((not (listp l)) nil)
    ((null l) t)
    ((atom? (car l)) (lat? (cdr l)))
    (t nil)
))

;; returns true if lat is list of atoms and a is present in lat
(defun member? (a lat)
  (cond
    ((not (listp lat)) nil) ;; we could also check for lat? here
    ((null lat) nil)
    (t (or (eq a (car lat)) (member? a (cdr lat))))
))

;; returns lat with a removed (if lat is list of atoms) otherwise just returns lat
;; NOTE: I am not sure what the behaviour should be if not lat...
(defun rember (a lat)
  (cond
    ((not (listp lat)) nil) ;; we could also check for lat? here
    ((null lat) ())
    ((eq a (car lat)) (cdr lat))
    (t (cons (car lat) (rember a (cdr lat))))
))

;; takes a list of lists as argument
;; returns a list with the first element of each list
(defun firsts (l)
  (cond
    ((not (listp l)) ())
    ((null l) ())
    (t (cons (car (car l)) (firsts (cdr l))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted after the first occurence of old
(defun insertR (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons (car lat) (cons new (cdr lat))))
    (t (cons (car lat) (insertR new old (cdr lat))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted before the first occurence of old
(defun insertL (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons new lat))
    (t (cons (car lat) (insertL new old (cdr lat))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted in place of the first occurence of old
(defun mysubst (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons new (cdr lat)))
    (t (cons (car lat) (mysubst new old (cdr lat))))
))

;; takes three atoms and a lat
;; returns a lat with new inserted in place of the first occurence of o1 or o2
(defun mysubst2 (new o1 o2 lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((or (eq o1 (car lat)) (eq o2 (car lat))) (cons new (cdr lat)))
    (t (cons (car lat) (mysubst2 new o1 o2 (cdr lat))))
))

;; takes an atom and a lat
;; returns lat with all occurences of a removed
(defun multirember (a lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq a (car lat)) (multirember a (cdr lat)))
    (t (cons (car lat) (multirember a (cdr lat))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted after every occurence of old
(defun multiinsertR (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (t (cons (car lat) (multiinsertR new old (cdr lat))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted before every occurence of old
(defun multiinsertL (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat))) ))
    (t (cons (car lat) (multiinsertL new old (cdr lat))))
))

;; takes two atoms and a lat
;; returns a lat with new inserted in place of every occurence of old
(defun multisubst (new old lat)
  (cond
    ((not (lat? lat)) nil)
    ((null lat) ())
    ((eq old (car lat)) (cons new (multisubst new old (cdr lat))))
    (t (cons (car lat) (multisubst new old (cdr lat))))
))

;; adds numbers
(defun add (a b)
  (cond
    ((zero? b) a)
    (t (add (1+ a) (1- b)))
))

;; subtract numbers
(defun sub (a b)
  (cond
    ((zero? b) a)
    (t (sub (1- a) (1- b)))
))

;; sum of the tup
(defun addtup (tup)
  (cond
    ((null tup) 0)
    (t (add (car tup) (addtup (cdr tup))))
))

(defun mult (a b)
  (cond
    ((zero? b) 0)
    (t (add a (mult a (1- b))))
))

;; la and lb are tuples of the same length
;; returns a tuple with sums of each index
(defun tup+ (la lb)
  (cond
    ((null la) lb)
    ((null lb) la)
    (t (cons (add (car la) (car lb)) (tup+ (cdr la) (cdr lb))))
))

;; greater than
(defun gt (a b)
  (cond
    ((zero? a) nil)
    ((zero? b) t)
    (t (gt (1- a) (1- b)))
))

;; lesser than
(defun lt (a b)
  (cond
    ((zero? b) nil)
    ((zero? a) t)
    (t (lt (1- a) (1- b)))
))

;; equal
(defun equa (a b)
  (cond 
    ((gt a b) nil)
    ((lt a b) nil)
    (t t)
))

;; return a power b
(defun pow (a b)
  (cond
    ((zero? b) 1)
    (t (mult a (pow a (1- b))))
))

(defun div (a b)
  (cond
    ((lt a b) 0)
    (t (1+ (div (sub a b) b)))
))

(defun len (lat)
  (cond 
    ((null lat) 0)
    (t (1+ (len (cdr lat))))
))

(defun pick (a lat)
  (cond
    ((zero? (1- a)) (car lat))
    (t (pick (1- a) (cdr lat)))
))

(defun rempick (a lat)
  (cond
    ((zero? (1- a)) (cdr lat))
    (t (cons (car lat) (rempick (1- a) (cdr lat))))
))

(defun nonums (l)
  (cond
    ((null l) ())
    ((numberp (car l)) (nonums (cdr l)))
    (t (cons (car l) (nonums (cdr l))))
))

(defun allnums (l)
  (cond
    ((null l) ())
    ((numberp (car l)) (cons (car l) (allnums (cdr l))))
    (t (allnums (cdr l)))
))

(defun eqan (a b)
  (cond
    ((and (numberp a) (numberp b)) (equa a b))
    ((and (atom? a) (atom? b)) (eq a b))
    (t nil)
))

(defun occur (a lat)
  (cond
    ((null lat) 0)
    ((eq a (car lat)) (1+ (occur a (cdr lat))))
    (t (occur a (cdr lat)))
))

(defun rember* (a l)
  (cond
    ((null l) ())
    ((listp (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
    ((eq a (car l)) (rember* a (cdr l)))
    (t (cons (car l) (rember* a (cdr l))))
))

(defun eqlist (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or  (null l1) (null l2)) nil)
    ((and (listp (car l1)) (listp (car l2))) (and (eqlist (car l1) (car l2)) (eqlist (cdr l1) (cdr l2))))
    ((and (atom? (car l1)) (atom? (car l2))) (and (eq (car l1) (car l2)) (eqlist (cdr l1) (cdr l2))))
    (t nil)
))

(defun num? (a)
  (cond
    ((atom? a) t)
    ((eq `+ (car (cdr a))) t)
))

(defun set? (l)
  (cond
    ((null l) t)
    ((member? (car l) (cdr l)) nil)
    (t (set? (cdr l)))
))

(defun makeset (l)
  (cond
    ((null l) ())
    (t (cons (car l) (makeset (multirember (car l) (cdr l)))))
))

(defun subset? (l1 l2)
  (cond
    ((null l1) t)
    (t (and (member? (car l1) l2) (subset? (cdr l1) l2)))
))

(defun eqset? (l1 l2)
  (and (subset? l1 l2) (subset? l2 l1))
)

(defun intersect? (l1 l2)
  (cond
    ((null l1) nil)
    (t (or (member? (car l1) l2) (intersect? (cdr l1) l2)))
))

(defun intersect (l1 l2)
  (cond
    ((null l1) ())
    ((member? (car l1) l2) (cons (car l1) (intersect (cdr l1) l2)))
    (t (intersect (cdr l1) l2))
))

(defun append_ (l1 l2)
  (cond
    ((null l1) l2)
    (t (cons (car l1) (append_ (cdr l1) l2)))
))

(defun union_ (l1 l2)
  (makeset (append_ l1 l2))
)

(defun intersectall (l)
  (cond
    ((null (cdr l)) (car l))
    (t (intersect (car l) (intersectall (cdr l))))
))

(defun ispair? (l)
  (cond
    ((not (listp l)) nil)
    ((null l) nil)
    ((null (cdr l)) nil)
    ((null (cdr (cdr l))) t)
    (t nil)
))

(defun first_ (l)
  (car l)
)

(defun second_ (l)
  (car (cdr l))
)

(defun build (a b)
  (cons a (cons b ()))
)

(defun fun? (rel)
  (set? (firsts rel))
)

(defun rev (pair)
  (build (second_ pair) (first_ pair))
)

(defun revrel (rel)
  (cond
    ((null rel) ())
    (t (cons (rev (car rel)) (revrel (cdr rel))))
))

(defun fullfun? (f)
  (and (fun? f) (fun? (revrel f)))
)

(defun remberf (test?)
  (function
    (lambda (a l)
      (cond
        ((null l) ())
        ((funcall test? a (car l)) (cdr l))
        (t (cons (car l) (funcall (remberf test?) a (cdr l))))
))))

(defun eq?-c (a)
  (function
    (lambda (x)
      (eq x a)))
)

(setq eq?-salad (eq?-c `salad))

(defun insertlf (test?)
  (function
    (lambda (new old l)
      (cond
        ((null l) ())
        ((funcall test? old (car l))
         (cons new (cons old (cdr l))))
        (t (funcall (insertlf test?) new old (cdr l)))
))))

(defun insertrf (test?)
  (function
    (lambda (new old l)
      (cond
        ((null l) ())
        ((funcall test? old (car l))
         (cons old (cons new (cdr l))))
        (t (funcall (insertlf test?) new old (cdr l)))
))))

;; if true insertl otherwise insertr
(defun insertg (bool)
  (function
    (lambda (a l)
      (cond 
        ((eq bool t) (cons a l))
        (t (cons (car l) (cons a (cdr l))))
))))

(defun insertf (test? l-or-r)
  (function
    (lambda (new old l)
      (cond
        ((null l) ())
        ((funcall test? old (car l))
         (funcall (insertg l-or-r) new l))
        (t (funcall (insertf test? l-or-r) new old (cdr l)))
))))

(defun counts (lat seen)
  (cons lat (cons (length seen) ()))
)

(defun multirember-co (a lat col)
  (cond
    ((null lat) (funcall col () ()))
    ((eq a (car lat))
     (multirember-co a (cdr lat)
                     (lambda (newlat seen)
                       (funcall col newlat (cons (car lat) seen)))))
    (t (multirember-co a (cdr lat)
                       (lambda (newlat seen)
                         (funcall col (cons (car lat) newlat) seen))))
))

(defun insert-counts (lat l r)
  (cons lat (cons l (cons r ())))
)

;; multiinsertlr-co
;; takes new, oldl, oldr, lat, collector as argumenrs
;; returns lat, lcount, rcount as result
(defun multiinsertlr-co (new oldl oldr lat col)
  (cond
    ((null lat) (funcall col () 0 0))
    ((eq oldl (car lat)) 
     (multiinsertlr-co new oldl oldr (cdr lat)
                       (lambda (newlat l r)
                         (funcall col (cons new (cons oldl newlat)) (1+ l) r))))
    ((eq oldr (car lat)) 
     (multiinsertlr-co new oldl oldr (cdr lat)
                       (lambda (newlat l r)
                         (funcall col (cons oldr (cons new newlat)) l (1+ r)))))
    (t (multiinsertlr-co new oldl oldr (cdr lat)
                         (lambda (newlat l r)
                           (funcall col (cons (car lat) newlat) l r))))
))

(defun even? (x)
  (= (* (div x 2) 2) x)
)

(defun evens-only* (l)
  (cond
    ((null l) ())
    ((listp (car l)) (cons (evens-only* (car l)) (evens-only* (cdr l))))
    ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
    (t (evens-only* (cdr l)))
))

(defun eocounter (evens o e)
  (cons evens (cons o (cons e ())))
)

;; checks all the numbers within nested lists
;; adds up odd numbers to o
;; multiplies up even numbers to e
;; collects even numbers
(defun evens-odds-co (l col)
  (cond
    ((null l) (funcall col () 0 1))
    ((listp (car l))
     (evens-odds-co (car l)
                    (lambda (evens o e)
                      (evens-odds-co (cdr l)
                                     (lambda (evens2 o2 e2)
                                      (funcall col (cons evens evens2) (+ o o2)  (* e e2)))))))
    ((even? (car l))
     (evens-odds-co (cdr l)
                    (lambda (evens o e)
                      (funcall col (cons (car l) evens) o (* e (car l))))))
    (t
     (evens-odds-co (cdr l)
                    (lambda (evens o e)
                      (funcall col evens (+ o (car l)) e))))
))

