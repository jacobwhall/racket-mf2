#lang racket

(require sxml
         html-parsing
         json
         gregor
         net/url
         racket/hash
         "structures.rkt")

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


(define (value-class-pattern element dt?) ; https://microformats.org/wiki/value-class-pattern
  (let ([vcp (let ([value-kid ((select-kids (λ (x) (find-class "value" x))) element)])
               (if (pair? value-kid)
                   (let ([n (sxml:element-name (car value-kid))]) ; TODO: correctly handle multiple matching children
                     (or (and (member n (list 'img 'area)) (find-attr 'alt value-kid))
                         (and (equal? n 'data) (or (find-attr 'value value-kid)
                                                   (sxml:text value-kid)))
                         (and (equal? n 'abbr) (find-attr 'value value-kid))
                         (sxml:text value-kid)))
                   #f))])
    (if vcp
        (if dt?
            (iso8601->datetime vcp)
            vcp)
        #f)))


(define (property->symbol property)
  (string->symbol (string-append (cadr (string-split property
                                                     "-")))))


(define (parse-p-* element class-list)
  (map (λ (class)
         (property 'p
                   (property->symbol class)
                   (list (let ([n (sxml:element-name element)])
                           (or (value-class-pattern element
                                                    #f)
                               (and (member n (list 'abbr 'link)) (find-attr 'title element))
                               (and (member n (list 'data 'input)) (find-attr 'value element))
                               (and (member n (list 'img 'area)) (find-attr 'alt element))
                               (sxml:text element))))
                   #f))
       class-list))


(define (parse-u-* element class-list)
  (map (λ (class)
         (property 'u
                   (property->symbol class)
                   (list (string->url (let ([n (sxml:element-name element)])
                                        (or (and (member n (list 'a 'area 'link)) (find-attr 'href element))
                                            (and (equal? n 'img) (find-attr 'src element)); TODO: if there is an [alt], we need to include it. see section 1.5
                                            (and (member n (list 'audio 'video 'source 'iframe)) (find-attr 'src element))
                                            (and (equal? n 'video) (find-attr 'poster element))
                                            (and (equal? n 'object) (find-attr 'data element))
                                            (and (member n (list 'audio 'video 'source 'iframe)) (find-attr 'src element))
                                            (value-class-pattern element
                                                                 #f)
                                            (and (equal? n 'abbr) (find-attr 'title element))
                                            (and (member n (list 'data 'input)) (find-attr 'value element))
                                            (sxml:text element)))))  ; TODO:  removing all leading/trailing whitespace and nested <script> & <style> elements beforehand
                   #f))
       class-list))


(define (parse-dt-* element class-list)
  (map (λ (class)
         (property 'dt
                   (property->symbol class)
                   (list (let ([n (sxml:element-name element)]) ; TODO: more permissive datetime parsing (also, allow for +0000 on iso8601 datetimes?)
                           (or (value-class-pattern element
                                                    #t)
                               (and (member n (list 'time 'ins 'del)) (find-attr 'datetime element))
                               (and (equal? n 'abbr) (find-attr 'title element))
                               (and (member n (list 'data 'input)) (find-attr 'value element))
                               (sxml:text element))))
                   #f))
       class-list))


(define (parse-e-* element class-list)
  (map (λ (class)
         (property 'e
                   (property->symbol class)
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


(define (process-duplicates properties)
  (let ([duplicate (check-duplicates properties
                                     #:key property-title)])
    (if duplicate
        (append (remove-duplicates properties
                                   #:key property-title)
                (list (property (property-prefix duplicate)
                                (property-title duplicate)
                                (foldr append
                                       '()
                                       (map property-value
                                            (filter (λ (p)
                                                      (equal? (property-title p)
                                                              (property-title duplicate)))
                                                    properties)))
                                (property-experimental duplicate))))
        properties)))


(define (recursive-parse elements)
  (map (λ (element)
         (let ([h-types (map (λ (s) (string->symbol (substring s 2)))
                             (find-class "h-.+" element))]
               [parsed-children (flatten (recursive-parse (sxml:child-elements element)))]
               [properties (parse-properties element)])
           (cond [(and (pair? h-types)
                       (pair? properties))
                  (property 'h
                            (property-title (car properties))
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
                 [else (process-duplicates (append properties
                                                   parsed-children))])))
       elements))


(define (parse-elements element-list)
  (filter microformat?
          (recursive-parse element-list)))

(define (element->rels element)
  (let ([href (find-attr 'href element)])
    (cons (if (list? href)
              (hasheq)
              (hasheq (string->symbol (find-attr 'rel element))
                      (list href)))
          (cons (string->symbol (find-attr 'href element))
                (make-hasheq (filter pair? (append (list (cons 'rels
                                                               (string-split (find-attr 'rel element))))
                                                   (map (λ (attr)
                                                          (let ([value (find-attr attr element)])
                                                            (if (list? value)
                                                                null
                                                                (cons attr
                                                                      value))))
                                                        (list 'hreflang
                                                              'media
                                                              'title))
                                                   (list (cons 'text
                                                               (sxml:text element))))))))))

(define (string->microformats input)
  (let ([input-doc (html->xexp input)])
    (let ([rel-pairs (map element->rels
                          ((sxpath "//*[@rel and local-name()='a' or local-name()='link' or local-name()='area']")
                           input-doc))])
      (println (map cdr rel-pairs))
      (make-hasheq (list (cons 'items
                               (map microformat->jsexpr
                                    (parse-elements (sxml:child-elements input-doc))))
                         (cons 'rels
                               (apply hash-union
                                      (map car
                                           rel-pairs)
                                      #:combine/key (lambda (k v1 v2)(append v1 v2))))
                         (cons 'rel-urls
                               (make-hasheq (map cdr rel-pairs))))))))