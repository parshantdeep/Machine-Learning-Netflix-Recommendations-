;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; *****************************************
;; Netflix Recommendation System
;; Author: Parshant Utam
;; *****************************************
;;


;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; requires: each Nat is either 0 or 1

;; A Rating is an Int
;; requires: Int is either -1 or 1

(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)


;; ==== Part A ============================================================
;;(negate list) produces a list with every number in list negated
;; negate: (listof Num) -> (listof Num)
;; Examples:
(check-expect (negate (list 1 -1 4 -5 -6 6)) (list -1 1 -4 5 6 -6))

(define (negate list)
  (cond
    [(empty? list) empty]
    [else (cons (-(first list)) (negate (rest list)))]))

;;(add-list list1 list2) produces a list of sums of corresponding items of list1 and list2
;; add-list: (listof Num) (listof Num) -> (list0f Num)
;; Examples:
(check-expect (add-list (list 1 2 3 4) (list 1 2 3 4)) (list 2 4 6 8))
(check-expect (add-list (list 1 2 3 4) (list -1 -2 -3 -4)) (list 0 0 0 0))

(define (add-list list1 list2)
  (cond
   [(or (empty? list1) (empty? list2)) empty]
   [else (cons (+ (first list1) (first list2)) (add-list (rest list1) (rest list2)))]))

(define zero-list (list 0 0 0 0 0 0 0 0))
        
;;(find-preference Rating Movie-fv) produces a list with 8 integers representing total score for all genres
;; find-preference: (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: both lists are the same size
;; both lists are non-empty
;; Examples:
(check-expect (find-preference (list 1 1 -1) (list (list 1 1 0 0 0 0 0 0)
                                                   (list 1 1 1 0 0 0 0 0)
                                                   (list 0 1 1 1 0 0 0 0)))
              (list 2 1 0 -1 0 0 0 0))

(define (find-preference Rating Movie-fv)
  (cond
    [(and (empty? Rating) (empty? Movie-fv)) zero-list]
    [(= (first Rating) -1) (add-list (negate (first Movie-fv)) (find-preference (rest Rating)
                                                                                (rest Movie-fv)))]
    [else (add-list (first Movie-fv) (find-preference (rest Rating) (rest Movie-fv)))]))


;; ==== Part B ============================================================

;;(dot-product list1 list2) calculates the dot-product of list1 and list2
;; dot-product: (listof Num) (listof Num) -> Num
;;  requires list1 and list2 to be of equal length
;; Examples:
(check-expect (dot-product (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)) 0)
(check-expect (dot-product (list 1 3 0 0 5 0) (list 1 -1 0 0 1 0)) 3)

(define (dot-product list1 list2)
  (cond
    [(and (empty? list1) (empty? list2)) 0]
    [else (+ (* (first list1) (first list2)) (dot-product (rest list1) (rest list2)))]))

;; (final-scores preference movies) produces a list of dot-products between preference vector
;;     and (movie-genres) of all the items in movies.
;; final-scores: Pref-v (listof Movie) -> (listof Num)
;;    requires Pref-v and (listof genres) is of same length
;; Examples:
(check-expect (final-scores (list 2 1 0 -1 0 0 0 0)
                            (list (make-movie "moon" (list 1 0 0 0 1 0 1 0))
(make-movie "Dog" (list 0 1 1 0 0 0 0 0))
(make-movie "Cat" (list 0 0 0 1 0 1 0 0)))) (list 2 1 -1)) 

(define (final-scores preference movies-list)
  (cond
    [(empty? movies-list) empty]
    [else (cons (dot-product preference (movie-genres (first movies-list)))
                (final-scores preference (rest movies-list)))]))

;;(max-list list) produces the maximum number in the list of Numbers
;; max-list: (listof Num) -> Num
;; Examples:
(check-expect (max-list (list 5 8 10 4 6 -1 2 35 12)) 35)
(check-expect (max-list (list -5 -8 -10 -4 -6 -1 -2 -35 -12)) -1)

(define (max-list list)
  (cond
    [(empty? (rest list)) (first list)]
    [else (max (first list) (max-list (rest list)))]))

;;(position list element) determines the position of the number in the list of numbers
;;   in the case of more than one required element in list, position of earliest number is given
;; position: (listof Num) Num -> Nat
;;   requires list to be non-empty
;; Example:
(check-expect (position (list 0 1 5 8 9) 9) 4)
(check-expect (position (list 0 1 9 8 9) 9) 2)
 
(define (position list element)
  (cond
    [(= (first list) element) 0]
    [else (+ 1 (position (rest list) element))]))

;; (best-movie movies-list position) takes out Movie from movies-list
;;           at the certain position
;; best-movie: (listof Movie) Nat -> Movie
;; Examples:
(check-expect (best-movie (list (make-movie "moon" (list 1 0 0 0 1 0 1 0))
(make-movie "Dog" (list 0 1 1 0 0 0 0 0))
(make-movie "Cat" (list 0 0 0 1 0 1 0 0))) 1)(make-movie "Dog" (list 0 1 1 0 0 0 0 0)))

(check-expect (best-movie (list (make-movie "moon" (list 1 0 0 0 1 0 1 0))
(make-movie "Dog" (list 0 1 1 0 0 0 0 0))
(make-movie "Cat" (list 0 0 0 1 0 1 0 0))) 0)(make-movie "moon" (list 1 0 0 0 1 0 1 0))) 


(define (best-movie movies-list position)
  (cond
    [(zero? position) (first movies-list)]
    [else (best-movie (rest movies-list) (sub1 position))]))

;; (max-position list) determines the position of maximum number in the list
;; max-position: (listof Num) -> Nat
;; requires list to be non-empty
;; Examples:
(check-expect (max-position (list 1 10 5 -5 4)) 1)
(check-expect (max-position (list -100 -10 -5 -5 -40)) 2)

(define (max-position list)
  (position list (max-list list)))

;;(suggestions preference movies-list) produces the title of the movie with highest score
;; suggestions: Pref-v (listof Movie) -> Str
;; requires: (listof Movie) is non-empty

(define (suggestions preference movies-list)
  (movie-title
   (best-movie movies-list (max-position (final-scores preference movies-list)))))

;;Tests:
(check-expect (suggestions
(list 2 1 0 -1 0 0 0 0)
(list (make-movie "The Hadron" (list 1 0 0 0 1 0 1 0))
(make-movie "largefoot" (list 0 1 1 0 0 0 0 0))
(make-movie "A moon is Born" (list 0 0 0 1 0 1 0 0)))) "The Hadron")
(check-expect (suggestions
(list 2 1 0 -1 0 4 0 0)
(list (make-movie "The Hadron" (list 1 0 0 0 1 0 1 0))
(make-movie "largefoot" (list 0 1 1 0 0 0 0 0))
(make-movie "A moon is Born" (list 0 0 0 1 0 1 0 0)))) "A moon is Born")
              


  
  
                     



  





