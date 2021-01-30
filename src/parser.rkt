#lang racket

;;; Allows to read the rules file and create the model to be executed.

(provide

 ; (function) Reads the source file and extracts its model
 ; string -> model
 parse-mcpd-file)


; ------------------------
; implementation

(require "model.rkt")
(require rackunit)

(define (parse-mcpd-file filename)
  (parse-sections (split-file-in-sections filename)))

(define (split-file-in-sections filename)

  ; Some initialisation...
  (let ((inside-modes-section #f)
        (inside-curfew-section #f)
        (inside-rules-section #f)
        (modes-section "")
        (curfew-section "")
        (rules-section "")
        (model '()))

    ; Utility function that sets the inside-*-section variables to the desired values.
    ; boolean, boolean, boolean -> void
    (define (set-inside-section! modes curfew rules)
      (set! inside-modes-section modes)
      (set! inside-curfew-section curfew)
      (set! inside-rules-section rules))

    ; Here we read every file of rules.file, we determine the current section (modes, rules),
    ;      we skip the empty lines and depending by de section, we create populate a mode or rule structure. 
    (for ((line (file->lines filename)))
      (cond
        
        ((string=? line "[modes]") (set-inside-section! #t #f #f))

        ((string=? line "[curfew]") (set-inside-section! #f #t #f))
      
        ((string=? line "[rules]") (set-inside-section! #f #f #t))

        ((string=? "" (string-trim line))
         #f)
      
        ((eq? #t inside-modes-section)
         (set! modes-section (string-append modes-section line "\n")))

        ((eq? #t inside-curfew-section)
         (set! curfew-section (string-append curfew-section line "\n")))

        ((eq? #t inside-rules-section)
         (set! rules-section (string-append rules-section line "\n")))

        (else (error (string-append "Not in a known section: " line)))))

    (list (string-split modes-section "\n")
          (string-split curfew-section "\n")
          (string-split rules-section "\n"))))

; Utility function that applies in succession changes to the model by parsing all the sections.
; list(string) -> model
(define (parse-sections sections)
  (let*
      ((with-modes (parse-modes-section (first sections) (empty-model)))
       (with-curfew (parse-curfew-section (second sections) with-modes))
       (with-rules (parse-rules-section (third sections) with-curfew)))
    with-rules))

; Utility function that parses the modes section and applies changes to the model.
; string, model
(define (parse-modes-section modes-section model)
  (define temporary-model model)
  (for ((line modes-section))
    (set! temporary-model (read-mode line temporary-model)))
  temporary-model)

; Utility function that parses the curfew section and applies changes to the model.
; string, model -> model
(define (parse-curfew-section curfew-section model)
  (define starts #f)
  (define ends #f)
  (define can-enter #f)
  (define can-leave #f)
  (for ((line curfew-section))
    (let
        ((parsed-line (read-curfew line)))
      (cond
        ((eq? (first parsed-line) 'starts)
         (set! starts (second parsed-line)))

        ((eq? (first parsed-line) 'ends)
         (set! ends (second parsed-line)))

        ((eq? (first parsed-line) 'rules)
         (set! can-enter (second parsed-line))
         (set! can-leave (third parsed-line))))))

  (add-curfew-section starts ends can-enter can-leave model))

; Utility function that parses the rules section and applies changes to the model.
; string, model -> model
(define (parse-rules-section rules-section model)
  (define can-enter #f)
  (define can-leave #f)
  (for ((line rules-section))
    (let
        ((parsed-line (read-rules line)))

      (cond
        ((eq? (first parsed-line) 'can-enter)
         (set! can-enter (second parsed-line)))

        ((eq? (first parsed-line) 'can-leave)
         (set! can-leave (second parsed-line))))))
  
  (add-rules-section can-enter can-leave model))

; Utility function that parses a mode line and gives back the retrieved information in a list.
; string, model -> model
(define (read-mode line model)
  (let* ((mode-pair (split-and-trim line ":"))
         (name (first mode-pair))
         (mode-rules (split-and-trim (second mode-pair) ","))
         (can-enter (string=? "can enter" (first mode-rules)))
         (can-leave (string=? "can leave" (second mode-rules))))

    (add-mode name can-enter can-leave model)))

; Utility function that parses a curfew line and gives back the retrieved information in a list.
; string -> list
(define (read-curfew line)
  (let* ((curfew-pair  (split-and-trim line ":"))
         (curfew-key (first curfew-pair))
         (curfew-value (second curfew-pair)))
      
    (cond
      ((string=? curfew-key "starts")
       (list 'starts (string->number curfew-value)))

      ((string=? curfew-key "ends")
       (list 'ends (string->number curfew-value)))

      ((string=? curfew-key "rules")
       (let* ((rules-pair (split-and-trim curfew-value ","))
              (can-enter (string=? "can enter" (first rules-pair)))
              (can-leave (string=? "can leave" (second rules-pair))))
         
         (list 'rules can-enter can-leave))))))
            
; Utility function that parses a rules line and gives back the retrieved information in a list.
; string -> list
(define (read-rules line)
  (let* ((rule-pair (split-and-trim line ":"))
         (name (cond
                 ((string=? (first rule-pair) "enter") 'can-enter)
                 ((string=? (first rule-pair) "leave") 'can-leave)))
         (rule-condition-text (second rule-pair))
         (rule-condition (if (string=? rule-condition-text "always") 'always 'when-registered)))
    
    (list name rule-condition)))

; Utility function to split a string while also trimming its parts
; string, string -> list(string)
(define (split-and-trim line separator)
  (map string-trim (string-split line separator)))


; ------------------------
; unit tests

(check-equal?
 (split-file-in-sections "rules.mcpd")
 '(("fully open: can enter, can leave"
    "only enter: can enter, cannot leave"
    "only leave: cannot enter, can leave"
    "fully closed: cannot enter, cannot leave"
    "default: can enter, cannot leave")
   ("starts: 22" "ends: 6" "rules: can enter, cannot leave")
   ("enter: when registered" "leave: always")))

(check-equal?
 (parse-modes-section
  '("fully open: can enter, can leave"
    "only enter: can enter, cannot leave"
    "only leave: cannot enter, can leave"
    "fully closed: cannot enter, cannot leave"
    "default: can enter, cannot leave")
  (empty-model))

 '((modes
    ("default" (can-enter #t) (can-leave #f))
    ("fully closed" (can-enter #f) (can-leave #f))
    ("only leave" (can-enter #f) (can-leave #t))
    ("only enter" (can-enter #t) (can-leave #f))
    ("fully open" (can-enter #t) (can-leave #t)))
   (curfew)
   (rules)))

(check-equal?
 (parse-curfew-section
  '("starts: 22" "ends: 6" "rules: can enter, cannot leave")
  (empty-model))

 '((modes)
   ((curfew
     (starts 22)
     (ends 6)
     (can-enter #t)
     (can-leave #f)))
   (rules)))

(check-equal?
 (parse-rules-section
  '("enter: when registered" "leave: always")
  (empty-model))
 
 '((modes)
   (curfew)
   ((rules
     (can-enter when-registered)
     (can-leave always)))))

(check-equal?
 (parse-mcpd-file "rules.mcpd")

 '((modes
    ("default" (can-enter #t) (can-leave #f))
    ("fully closed" (can-enter #f) (can-leave #f))
    ("only leave" (can-enter #f) (can-leave #t))
    ("only enter" (can-enter #t) (can-leave #f))
    ("fully open" (can-enter #t) (can-leave #t)))
   
   ((curfew
     (starts 22)
     (ends 6)
     (can-enter #t)
     (can-leave #f)))
    
   ((rules
     (can-enter when-registered)
     (can-leave always)))))