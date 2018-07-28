#lang racket

(require racket/runtime-path racket/gui/base drracket/tool drracket/tool-lib framework racket/fasl)
(provide tool@)

(module backend racket/base
  (require racket/place racket/port racket/fasl scribble/xref scribble/manual-struct)
  (provide run)
  
  (define (run pch)
    (place-channel-put pch (load)))

  (define (load)
    (define data
      (call-with-output-bytes
       (λ (p)
         (s-exp->fasl (get-completions/manuals) p))))
    (define shared (make-shared-bytes (bytes-length data)))
    (bytes-copy! shared 0 data)
    shared)
  
  (define (get-completions/manuals)
    (define xref
      (let ([load-collections-xref
             ;; Make the dependency on `setup/xref' indirect, so that a
             ;; GUI does not depend on having documentation installed:
             (with-handlers ([exn:missing-module? (lambda (exn)
                                                    (lambda ()
                                                      (load-xref null)))])
               (dynamic-require 'setup/xref 'load-collections-xref))])
        (load-collections-xref)))
    
    (let ([ht (make-hash)])
      (for-each
       (λ (entry)
         (let ([desc (entry-desc entry)])
           (when (exported-index-desc? desc)
             (let ([name (exported-index-desc-name desc)])
               (when name
                 (hash-set! ht (symbol->string name) #t))))))
       (xref-index xref))
      (sort (hash-map ht (λ (x y) x)) string<?))))

(define pch #f)
(define symbols #f)

(define (load-symbols)
  (cond
    [symbols symbols]
    [pch (set! symbols 
               (call-with-input-bytes
                (place-channel-get pch)
                fasl->s-exp))
         (set! pch #f)
         symbols]
    [else (start-backend)
          (load-symbols)]))

(define-runtime-path me "tool.rkt")

(define (start-backend)
  (unless pch
    (set! pch (dynamic-place `(submod ,me backend) 'run))))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))
    (define (phase2) (void))

    (define prefetch-mixin
      (mixin (text:autocomplete<%>) ()
        
        (define/override (get-all-words)
          (load-symbols))
        
        (super-new)

        (thread
         (λ ()
           (sync (system-idle-evt))
           (queue-callback start-backend)))
        
        ))
    
    (drracket:get/extend:extend-definitions-text prefetch-mixin)
    (drracket:get/extend:extend-interactions-text prefetch-mixin)
    ))
