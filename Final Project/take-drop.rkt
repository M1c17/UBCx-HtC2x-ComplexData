;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname take-drop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; lon Natural -> lon
;; produce after dropping first n element
;; ASSUME: lon has at least n element
(check-expect (drop empty 0) empty)
(check-expect (drop (list 2 3 4 5) 0) (list 2 3 4 5))
(check-expect (drop (list 2 3 4 5) 2) (list 4 5))
(check-expect (drop (list 1 3 5 6) 3) (list 6))

;(define (drop lon n) empty)  ;stub

; Template self recursive
(define (drop lonO n)
  ;;acc: listofx; context preserving; keep track of the elements of the list seen so far  
  (local [(define (drop lon acc)
            (cond [(empty? lon) empty]
                  [else
                   (if (= 0 acc)
                       lon
                       (drop (rest lon) (sub1 acc)))]))]
    (drop lonO n)))

;; lon Natural -> lon
;; produce list of first n elements of lon
;; ASSUME: lon has at least n element
;; !!!!
(check-expect (take empty 0) empty)
(check-expect (take (list 2 3 4 5) 0) empty)
(check-expect (take (list 2 3 4 5) 2) (list 2 3))
(check-expect (take (list 5 6 7 8) 3) (list 5 6 7))
(check-expect (take (list 3 6 7) 5) (list 3 6 7))

;(define (take lon n) empty)  ;stub

(define (take lonO n)
  ;; acc: listofx ; keep track of the elemeth already seen
  ;; (list 1 2 3) 2) ; outer function
  ;; (list 1 2 3) 2) ; take
  ;; (list   2 3) 1) ; take
  ;; (list     3) 0) ; no take
  (local [(define (take lon acc)
          (cond [(empty? lon) empty]
                [else
                 (if (= 0 acc)
                     empty
                     (cons (first lon)(take (rest lon) (sub1 acc))))]))]
    (take lonO n)))