#lang racket

(require advent-of-code
         racket/runtime-path
         threading
         sugar)

(define-runtime-path here ".")

(define session-key
"53616c7465645f5f544b8d07e445ed446fb846b92a541c8ebd5012cad71c48e02fbc146cdc8ab1e862\
ee472841c606c41644abae95ed6368dcfc0630c8f127bb")

(string-length session-key)

(define input
  (~>
   (port->lines (open-aoc-input session-key 2024 02 #:cache (build-path here "input")))
   (map (compose (λ (lon) (map string->number lon)) string-split) _)))

(define test-input
(map (compose (λ (lon) (map string->number lon)) string-split)
(regexp-split #px"\n"
 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")))

(define (dampener-safe lon)
  ;; removing the first or last never makes a safe one unsafe
  (for/or ([idx (in-range (length lon))])
    (safe (append (take lon idx) (drop lon (add1 idx))))))

(define (safe lon)
  (define diffs (lon-diffs lon))
  (and (or (all-positive diffs)
           (all-negative diffs))
       (andmap (λ (n) (< 0 (abs n) 4)) diffs)))

(define (all-positive lon)
  (andmap (λ (n) (< 0 n)) lon))

(define (all-negative lon)
  (andmap (λ (n) (< n 0)) lon))

(define (lon-diffs lon)
  (for/list ([a (in-list lon)]
        [b (in-list (rest lon))])
    (- b a)))


(define answer (count safe input))

(define answer2 (count dampener-safe input))

(count dampener-safe test-input)

(aoc-submit session-key 2024 2 2 answer2)
#;(




(define a (map (compose string->number first) input))
(define b (map (compose string->number second) input))

(define b-freq-hash (frequency-hash b))
(define s1 (sort a <))
(define s2 (sort b <))

(define answer2
  (for/sum ([i (in-list a)])
    (* i (hash-ref b-freq-hash i 0))))

answer2

(define answer (apply + (map (λ (a b) (abs (- a b))) s1 s2)))

;(aoc-submit session-key 2024 1 1 answer)
)