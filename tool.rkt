#lang racket

(require racket/gui/base drracket/tool drracket/tool-lib framework racket/fasl)
(provide tool@)

(define cached-file (build-path (find-system-path 'home-dir) ".cached-autocomplete"))

(define loaded #f)

(define (remove-cached)
  (with-handlers
      ([(λ (_) #t) void])
    (delete-file cached-file)))

(define (load-cached)
  (when (and (not loaded) (file-exists? cached-file))
    (with-handlers
        ([(λ (_) #t) (λ (_) (remove-cached))])
      (call-with-input-file cached-file
        (λ (p)
          (set! loaded (fasl->s-exp p))))))
  loaded)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))
    (define (phase2) (void))

    (define cached-mixin
      (mixin (text:autocomplete<%>) ()
        (define/override (get-all-words)
          (or (load-cached)
              (begin
                (set! loaded (super get-all-words))
                (call-with-output-file cached-file
                  (λ (p)
                    (s-exp->fasl loaded p))
                  #:exists 'replace)
                loaded)))
        (super-new)))

    (define invalidate-mixin
      (mixin (drracket:frame:<%>) ()
        (super-new)
        (new menu-item% [label "Invalidate Cached Autocomplete"]
             [parent (send this get-show-menu)]
             [callback (λ (m e) (remove-cached)
                         (set! loaded #f))])))
    
    (drracket:get/extend:extend-definitions-text cached-mixin)
    (drracket:get/extend:extend-interactions-text cached-mixin)
    (drracket:get/extend:extend-unit-frame invalidate-mixin)
    ))
