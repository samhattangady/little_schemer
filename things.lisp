;; things.lisp is a file containing all the functions that the
;; book little_schemer defines over the course of the chapters
;; since the book builds on previous things, but the repl dies
;; we want a single source for all the things.

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
