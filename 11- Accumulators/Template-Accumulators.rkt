;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Template-Accumulators) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; Template for context preserving accumulators

(define (skip1 loxO)
  ;; acc:  
  (local [(define (skip1 lox acc)                  ; wrapping function in outers function,local
            (cond [(empty? lox) (...acc)]
                  [else
                   (...acc
                       (first lox)
                       (skip1 (rest lox)(...acc)))]))]
    (skip1 lox...)))                               ; trampoline ;Initialize accumulator

;; Template for mutually recursive

#;
(define (fn-for-wiz w)
  (local [(define (fn-for-wizard w)
            (... (wiz-name w)
                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))

          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wizard (first low))
                        (fn-for-low (rest low)))]))]
    (fn-for-wizard w)))