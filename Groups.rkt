;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Groups) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Higher-Level Math Definitions, The FUNctional Way ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GROUP THEORY ;;

(define-struct pair [first second])
;; group? :: Pair -> Boolean
;; is P a group?
(define (group? P)
  (and (group-pair? P)
       (local
         ((define set (pair-first P))
          (define binop (pair-second P))
          (define e (get-identity set binop)))
         (and
          (closed? set binop)
          (associative? set binop)
          (identity? set binop)
          (inverses? set binop)))))

;; closed? :: Set BinaryOperation -> Boolean
;; for all fixed s' in S, for all s in S, is O(s', s) in S?
(define (closed? S O)
  (andmap (λ (sPrime)
            (andMap (λ (s) (member? (O sPrime s) S)) S)) S))

;; associative? :: Set BinaryOperation -> Boolean
;; for all s1, s2, s3 in S, does O(O(s1, s2), s3) = O(s1, O(s2, s3))?
(define (associative? S O)
  (andmap (λ (s1)
            (andmap (λ (s2)
                      (andmap (λ (s3)
                                (eq? (O (O s1 s2) s3) (O s1 (O s2 s3)))) S)) S)) S))

;; identity? :: Set BinaryOperation -> Boolean
;; does there exist some e in S such that for all s in S O(e, s) = O(s, e) = s?
(define (identity? S O)
  (ormap (λ (s) (is-group-identity? s S O)) S))

;; is-group-identity? :: Real Set BinaryOperation -> Boolean
;; for some e, for all s in S, does O(e, s) = O(s, e) = s?
(define (is-group-identity? e S O)
  (andmap (λ (s) (and (eq? (O e s) (O s e)) (eq? (O s e) s))) S)) 

;; inverses? :: Real Set BinaryOperation -> Boolean
;; for all s in S, does there exist s^-1 in S such that O(s, s^-1) = O(s^-1, s) = e, for identity e
(define (inverses? e S O)
  (andmap (λ (s) (has-inverse? s e S O)) S))

;; has-inverse? :: Real Real Set BinaryOperation -> Boolean
;; for a fixed s' in S, is there one other s in S such that O(s, s^-1) = O(s^-1, s) = e?
(define (has-inverse sPrime e S O)
  (ormap (λ (si) (and (eq? (O si s) (O s si)) (eq? (O si s) e))) S))

;; group-candidate? :: Pair -> Boolean
;; does the pair P consist of a set and a binary operation in that order?
(define (group-candidate? P)
  (and (set? (first P))
       (binary-operation? (second P))))

;; set? :: Any -> Boolean
;; is the given parameter X a set? i.e. is a X a list that contains no duplicates?
(define (set? X)
  (and (cons? X)
       (not (contains-duplicates? X))))

;; binary-operation? :: 

;; contains-duplicates? :: <Real> [Real] -> Boolean
;; does the list X contain any duplicates?
(define (contains-duplicates? X)
  (#|cond [(empty? X) #f]
        [(cons? X) (or (member? (first X) (rest X))
                       (contains-duplicates? (rest X)))])|# (foldr (λ (x y) (or x y)) #t '(#t #t))))

