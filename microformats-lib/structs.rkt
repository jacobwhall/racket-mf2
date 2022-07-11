#lang racket

(require json
         gregor
         net/url)

(struct property
  (prefix
   title
   value
   experimental)
  #:transparent)

(struct microformat
  (types
   properties
   children
   experimental)
  #:transparent)

(define (microformat-type-strings mf)
  (map (λ (type)
         (string-append (if (microformat-experimental mf)
                            "h-x-"
                            "h-")
                        (symbol->string type)))
       (microformat-types mf)))

(define (microformat->jsexpr x)
  (cond [(microformat? x) (make-hasheq (filter (λ (y) (or (pair? (cdr y)) (hash? (cdr y))))
                                               (list (cons 'type
                                                           (microformat-type-strings x))
                                                     (cons 'properties
                                                           
                                                           (make-hasheq (map microformat->jsexpr
                                                                             (microformat-properties x))))
                                                     (cons 'children
                                                           (map microformat->jsexpr
                                                                (microformat-children x))))))]
        [(property? x) (cons (property-title x)
                             (cond ; [(p-property? x)] ; for now, use the else statement
                               [(equal? (property-prefix x) 'u)
                                (map url->string
                                     (property-value x))]
                               [(equal? (property-prefix x) 'dt)
                                (map datetime->iso8601
                                     (property-value x))]
                               [(equal? (property-prefix x) 'e)
                                (property-value x)]
                               [else 
                                (map (λ (v)
                                       (if (string? v)
                                           v
                                           (microformat->jsexpr v)))
                                     (property-value x))]))]))
      


                        

(provide
 (contract-out
  [struct property ((prefix (or/c 'p 'u 'dt 'e 'h))
                    (title symbol?)
                    (value (or/c (or/c (listof string?) (listof microformat?))
                                 (listof datetime?)
                                 (listof url?)
                                 (listof (hash/c (or/c 'html
                                                       'value)
                                                 string?))))
                    (experimental boolean?))]
  [struct microformat ((types (listof symbol?))
                       (properties (listof property?))
                       (children (listof microformat?))
                       (experimental boolean?))]
  [microformat->jsexpr (microformat? . -> . jsexpr?)]
  ))