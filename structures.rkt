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
  (id
   types
   properties
   value
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
  (cond [(microformat? x) (make-hasheq (filter (λ (y) (or (pair? (cdr y)) (hash? (cdr y)) (string? (cdr y))))
                                               (list (cons 'id
                                                           (microformat-id x))
                                                     (cons 'type
                                                           (sort (microformat-type-strings x) string<?))
                                                     (cons 'properties
                                                           (make-hasheq (map microformat->jsexpr
                                                                             (microformat-properties x))))
                                                     (cons 'value
                                                           (if (microformat-value x)
                                                               (cadr (microformat->jsexpr (microformat-value x)))
                                                               #f))
                                                     (cons 'children
                                                           (map microformat->jsexpr
                                                                (microformat-children x))))))]
        [(property? x) (cons (property-title x)
                             (cond
                               [(equal? (property-prefix x) 'u)
                                (map (λ (v)
                                       (if (hash? v)
                                           (hasheq 'alt (hash-ref v 'alt)
                                                   'value (url->string (hash-ref v 'value)))
                                           (url->string v)))
                                     (property-value x))]
                               [(equal? (property-prefix x) 'dt)
                                (map (λ (v)
                                       (if (datetime? v)
                                           (datetime->iso8601 v)
                                           v))
                                     (property-value x))]
                               [(equal? (property-prefix x) 'e)
                                (property-value x)]
                               [else 
                                (map (λ (v)
                                       (if (string? v)
                                           v
                                           (microformat->jsexpr v)))
                                     (property-value x))]))]))

(define property-value/c
  (or/c (listof string?)
        (listof microformat?)
        (listof (or/c string?
                      datetime?))
        (listof url?)
        (listof (hash/c (or/c 'html
                              'value)
                        string?))
        (listof (or/c (hash/c (or/c 'alt
                              'value)
                              (or/c string?
                              url?))
                      url?))))

(provide
 (contract-out
  [struct property ((prefix (or/c 'p 'u 'dt 'e 'h))
                    (title symbol?)
                    (value property-value/c)
                    (experimental boolean?))]
  [struct microformat ((id (or/c string? #f))
                       (types (listof symbol?))
                       (properties (listof property?))
                       (value (or/c property? #f))
                       (children (listof microformat?))
                       (experimental boolean?))]
  [microformat->jsexpr (microformat? . -> . jsexpr?)]))