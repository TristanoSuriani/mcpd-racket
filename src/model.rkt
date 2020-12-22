#lang racket

;;; Defines the model

(provide
 (struct-out model)
 (struct-out mode)
 (struct-out curfew)
 (struct-out rule)
 model-mode-from-mode-name)


(struct model
  ([modes]
   [curfew]
   [rules])
  #:transparent)

(struct mode
  ([name]
   [can-enter]
   [can-leave])
  #:transparent)

(struct curfew
  ([starts]
   [ends]
   [can-enter]
   [can-leave])
  #:transparent)

(struct rule
  ([name]
   [condition])
  #:transparent)

(define (model-mode-from-mode-name model the-mode-name)
  (first
   (filter
    (λ (current-mode) (string=? the-mode-name (mode-name current-mode)))
    (model-modes model))))