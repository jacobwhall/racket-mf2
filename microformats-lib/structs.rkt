#lang racket

(require json
         gregor
         net/url)

(struct property
  (title
   value
   experimental)
  #:transparent)

(struct p-property
  property
  ())

(struct u-property
  property
  ())

(struct dt-property
  property
  ())

(struct e-property
  property
  ())

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
                                               (list (cons 'types
                                                           (microformat-type-strings x))
                                                     (cons 'properties
                                                           
                                                           (make-hasheq (map microformat->jsexpr
                                                                             (microformat-properties x))))
                                                     (cons 'children
                                                           (map microformat->jsexpr
                                                                (microformat-children x))))))]
        [(property? x) (cons (property-title x)
                             (cond ; [(p-property? x)] ; for now, use the else statement
                               [(u-property? x)
                                (map url->string
                                     (property-value x))]
                               [(dt-property? x)
                                (map datetime->iso8601
                                     (property-value x))]
                               [(e-property? x)
                                (property-value x)]
                               [else 
                                (map (λ (v)
                                       (if (string? v)
                                           v
                                           (microformat->jsexpr v)))
                                     (property-value x))]))]))
      

(provide
 (contract-out
  [struct property ((title symbol?)
                    (value (or/c (listof string?) (listof microformat?)))
                    (experimental boolean?))]
  [struct p-property ((title symbol?)
                      (value (or/c (listof string?) (listof microformat?)))
                      (experimental boolean?))]
  [struct u-property ((title symbol?)
                      (value (listof url?))
                      (experimental boolean?))]
  [struct dt-property ((title symbol?)
                       (value (listof datetime?))
                       (experimental boolean?))]
  [struct e-property ((title symbol?)
                      (value (listof (hash/c (or/c 'html
                                                   'value) string?)))
                      (experimental boolean?))]
  [struct microformat ((types (listof symbol?))
                       (properties (listof property?))
                       (children (listof microformat?))
                       (experimental boolean?))]
  [microformat->jsexpr (microformat? . -> . jsexpr?)]
  ))