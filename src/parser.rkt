#lang racket

;;; Allows to read the rules file and create the model to be executed.

(provide

 ; [function] Reads the source file and extracts its model
 ; string -> model
 parse-mcpd-file)


; ------------------------
; implementation

(require "model.rkt")

(define (parse-mcpd-file filename)

  ; Some initialisation...
  (let ([modes-section #f]
        [curfew-section #f]
        [rules-section #f]
        
        [modes '()]
        [curfew-definition (curfew '() '() '() '())]
        [rules '()])

    ; Here we read every file of rules.file, we determine the current section (modes, rules),
    ;      we skip the empty lines and depending by de section, we create populate a mode or rule structure. 
    (for ([line (file->lines filename)])
      (cond
        
        ([string=? line "[modes]"]
         (begin
           (set! modes-section #t)
           (set! curfew-section #f)
           (set! rules-section #f)))

        ([string=? line "[curfew]"]
         (begin
           (set! modes-section #f)
           (set! curfew-section #t)
           (set! rules-section #f)))
      
        ([string=? line "[rules]"]
         (begin
           (set! modes-section #f)
           (set! curfew-section #f)
           (set! rules-section #t)))

        ([string=? "" (string-trim line)]
         #f)
      
        ([eq? #t modes-section]
         (set! modes (append modes (list (read-mode line)))))

        ([eq? #t curfew-section]
         (set! curfew-definition (read-curfew line curfew-definition)))

        ([eq? #t rules-section]
         (set! rules (append rules (list (read-rule line)))))

        (else (displayln line))))

    (model modes curfew-definition rules)))

; We are in the 'modes' section. Here we parse the mode line and populate a mode structure.
(define (read-mode line)
  (let* ([mode-pair (split-and-trim line ":")]
         [name (first mode-pair)]
         [mode-rules (split-and-trim (second mode-pair) ",")]
         [can-enter (string=? "can enter" (first mode-rules))]
         [can-leave (string=? "can leave" (second mode-rules))])

    (mode name can-enter can-leave)))

(define (read-curfew line curfew-definition)
  (let* ([curfew-pair  (split-and-trim line ":")]
         [curfew-key (first curfew-pair)]
         [curfew-value (second curfew-pair)])
      
    (cond
      ([string=? curfew-key "starts"]
       (curfew (string->number curfew-value)
               (curfew-ends curfew-definition)
               (curfew-can-enter curfew-definition)
               (curfew-can-leave curfew-definition)))

      ([string=? curfew-key "ends"]
       (curfew (curfew-starts curfew-definition)
               (string->number curfew-value)
               (curfew-can-enter curfew-definition)
               (curfew-can-leave curfew-definition)))

      ([string=? curfew-key "rules"]
       (let* ([rules-pair (split-and-trim curfew-value ",")]
              [can-enter (string=? "can enter" (first rules-pair))]
              [can-leave (string=? "can leave" (second rules-pair))])
         
         (curfew (curfew-starts curfew-definition)
                 (curfew-ends curfew-definition)
                 can-enter
                 can-leave))))))
            

; We are in the 'rules' section. Here we parse the rule line and populate a rule structure.
(define (read-rule line)
  (let* ([rule-pair (split-and-trim line ":")]
         [name (first rule-pair)]
         [rule-condition-text (second rule-pair)]
         [rule-condition (if (string=? rule-condition-text "always") 'always 'when-registered)])
           
    (rule name rule-condition)))

; Utility function to split a string while also trimming its parts
(define (split-and-trim line separator)
  (map string-trim (string-split line separator)))
