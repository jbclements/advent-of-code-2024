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
   (port->lines (open-aoc-input session-key 2024 01 #:cache (build-path here "input")))
   (map string-split _)))

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