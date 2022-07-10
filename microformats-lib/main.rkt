#lang racket

(require sxml
         html-parsing
         json
         gregor
         net/url
         "structs.rkt")

(provide (contract-out [string->microformats (string? . -> . jsexpr?)]))


(define (find-attr attr-name
                   element)
  (let ([attr-list (sxml:attr-list element)])
    (let ([result
           (assoc attr-name
                  attr-list)])
      (cond [result (cadr result)]
            [else null]))))
  

(define (find-class pattern
                    element)
  (let ([class (find-attr 'class element)])
    (filter (λ (x)
              (regexp-match (regexp pattern)
                            x))
            (regexp-split #rx" "
                          (cond [(null? class) ""]
                                [else class])))))


(define (value-class-pattern element) ; https://microformats.org/wiki/value-class-pattern
  (let ([value-kid ((select-kids (λ (x) (find-class "value" x))) element)])
    (if (pair? value-kid)
        (let ([n (sxml:element-name (car value-kid))]) ; TODO: correctly handle multiple matching children
          (or (and (member n (list 'img 'area)) (find-attr 'alt value-kid))
              (and (equal? n 'data) (or (find-attr 'value value-kid)
                                        (sxml:text value-kid)))
              (and (equal? n 'abbr) (find-attr 'value value-kid))
              (sxml:text value-kid)))
        #f)))


(define (property->symbol property)
  (string->symbol (string-append (cadr (string-split property
                                                     "-")))))

(define (parse-p-* element class-list)
  (map (λ (class)
         (p-property (property->symbol class)
                     (list (let ([n (sxml:element-name element)])
                             (or (value-class-pattern element)
                                 (and (member n (list 'abbr 'link)) (find-attr 'title element))
                                 (and (member n (list 'data 'input)) (find-attr 'value element))
                                 (and (member n (list 'img 'area)) (find-attr 'alt element))
                                 (sxml:text element))))
                     #f))
       class-list))

(define (parse-u-* element class-list)
  (map (λ (class)
         (u-property (property->symbol class)
                     (list (string->url (let ([n (sxml:element-name element)])
                                          (or (and (member n (list 'a 'area 'link)) (find-attr 'href element))
                                              (and (equal? n 'img) (find-attr 'src element)); TODO: if there is an [alt], we need to include it. see section 1.5
                                              (and (member n (list 'audio 'video 'source 'iframe)) (find-attr 'src element))
                                              (and (equal? n 'video) (find-attr 'poster element))
                                              (and (equal? n 'object) (find-attr 'data element))
                                              (and (member n (list 'audio 'video 'source 'iframe)) (find-attr 'src element))
                                              (value-class-pattern element)
                                              (and (equal? n 'abbr) (find-attr 'title element))
                                              (and (member n (list 'data 'input)) (find-attr 'value element))
                                              (sxml:text element)))))  ; TODO:  removing all leading/trailing whitespace and nested <script> & <style> elements beforehand
                     #f))
       class-list))


(define (parse-dt-* element class-list)
  (map (λ (class)
         (dt-property (property->symbol class)
                      (list (iso8601->datetime (let ([n (sxml:element-name element)]) ; TODO: more permissive datetime parsing (also, allow for +0000 on iso8601 datetimes?)
                                                 (or (value-class-pattern element)
                                                     (and (member n (list 'time 'ins 'del)) (find-attr 'datetime element))
                                                     (and (equal? n 'abbr) (find-attr 'title element))
                                                     (and (member n (list 'data 'input)) (find-attr 'value element))
                                                     (sxml:text element)))))
                      #f))
       class-list))


(define (parse-e-* element class-list)
  (map (λ (class)
         (e-property (property->symbol class)
                     (list (make-hasheq (list (cons 'value
                                                    (car (sxml:content element))) ; TODO: proper html serialization (), remove leading/trailing whitespace
                                              (cons 'html
                                                    (sxml:text element)))))
                     #f)) 
       class-list)) ; TODO (make sure to drop nested script, style tags; replace images with alt or src, removing leading/trailing whitespace)

(define (parse-properties element)
  (append
   (parse-p-* element (find-class "p-.+" element))
   (parse-u-* element (find-class "u-.+" element))
   (parse-dt-* element (find-class "dt-.+" element))
   (parse-e-* element (find-class "e-.+" element))))

(define (recursive-parse elements)
  (map (λ (element)
         (let ([h-types (map (λ (s) (string->symbol (substring s 2)))
                             (find-class "h-.+" element))]
               [parsed-children (flatten (recursive-parse (sxml:child-elements element)))]
               [properties (parse-properties element)])
           (cond [(and (pair? h-types)
                       (pair? properties))
                  (property (property-title (car properties))
                            (list (microformat h-types
                                               (filter property? parsed-children) ; TODO: add implied properties to this list
                                               (filter microformat? parsed-children)
                                               #f))
                            #f)]
                 [(pair? h-types)
                  (microformat h-types
                               (filter property? parsed-children) ; TODO: add implied properties to this list
                               (filter microformat? parsed-children)
                               #f)]
                 [else (append properties
                               parsed-children)])))
       elements))

(define (parse-elements element-list)
  (filter microformat?
          (recursive-parse element-list)))

(define (string->microformats input)
  (jsexpr->string (make-hasheq (list (cons 'items
                                           (map microformat->jsexpr
                                                (parse-elements (sxml:child-elements (html->xexp input)))))
                                     (cons 'rels
                                           (make-hasheq))
                                     (cons 'rel-urls
                                           (make-hasheq))
                                     ))))