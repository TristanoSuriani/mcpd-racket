#lang racket

;;; Defines the model for the MCPD domain specific language.

(provide

 ; [Structure] The model's root object and bindings (such as selectors).
 ; model: list<mode>, curfew, list<rules>
 (struct-out model)

 ; [Structure] A selectable mode which defines the operations a registered cat is allowed to do, and bindings.
 ; mode: string, boolean, boolean
 (struct-out mode)

 ; [Structure] A special mode that overrides the selected mode in a set period of time, and bindings.
 ; curfew: integer, integer, boolean, boolean
 (struct-out curfew)

 ; [Structure] A named rule with special values to define the business logic in a broader sense, and bindings.
 ; rule: string, symbol
 (struct-out rule)

 ; [Function] Utility function that allows to extract a mode using its name
 ; model, string -> mode
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