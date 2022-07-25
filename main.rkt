#lang racket

(require xml
         sxml
         html-parsing
         json
         gregor
         gregor/time
         net/url
         racket/hash
         "structures.rkt")

(provide (contract-out [string->microformats (string? url? . -> . jsexpr?)]))


(define (is-valid-url? href-string)
  (if (regexp-match url-regexp
                    href-string)
      (url-host (string->url href-string))
      #f))


(define (parse-url url-string
                   base-url)
  (combine-url/relative base-url
                        url-string))


(define (find-attr attr-name
                   element
                   #:noblank [noblank #f])
  (let ([result 
         (let ([attr-list (sxml:attr-list element)])
           (let ([result
                  (assoc attr-name
                         attr-list)])
             (cond [result (cadr result)]
                   [else null])))])
    (if (and noblank
             (equal? result ""))
        null
        result)))


(define (if-attr attr-name
                 element
                 #:noblank [noblank #f])
  (let ([find-attr-result (find-attr attr-name
                                     element
                                     #:noblank noblank)])
    (if (null? find-attr-result)
        #f
        find-attr-result)))


(define (find-class pattern
                    element)
  (let ([class (find-attr 'class element)])
    (filter (λ (x)
              (regexp-match (regexp pattern)
                            x))
            (regexp-split #rx" "
                          (cond [(null? class) ""]
                                [else class])))))


(define (find-h-types element)
  (map (λ (s) (string->symbol (substring s 2)))
       (find-class "^h(-[a-z0-9]+)?(-[a-z]+)+$" element)))


(define (find-only-child element)
  (let ([children (sxml:child-elements element)])
    (if (and (pair? children)
             (null? (cdr children)))
        (if (null? (find-h-types (car children)))
            (car children)
            #f)
        #f)))

(define (text-content element
                      base-url
                      #:notrim [notrim #f]
                      #:expand-imgs [expand-imgs #f])
  ((if notrim
       identity
       string-trim) (apply string-append
                           (map (λ (n) (cond [(string? n) n]
                                             [(and (sxml:element? n)
                                                   (equal? (sxml:element-name n) 'img)
                                                   (or (if-attr 'alt n)
                                                       (if-attr 'src n)))
                                              (string-join (string-split (string-trim (if (if-attr 'alt n)
                                                                                          (if-attr 'alt n)
                                                                                          (url->string (parse-url (if-attr 'src n)
                                                                                                                  base-url)))))
                                                           #:before-first (if expand-imgs " " "")
                                                           #:after-last (if expand-imgs " " ""))]
                                             [(and (sxml:element? n)
                                                   (not (member (sxml:element-name n) (list 'style 'script))))
                                              (text-content n
                                                            base-url
                                                            #:notrim #t)]
                                             [else ""]))
                                (sxml:content element)))))


(define (recursive-url-parse element
                             base-url)
  (if (sxml:element? element)
      (sxml:change-content
       (cond [(sxml:attr element 'href)
              (sxml:set-attr element
                             (list 'href
                                   (url->string (parse-url (car (string-split (sxml:attr element 'href)))
                                                           base-url))))]
             [(and (equal? (sxml:element-name element) 'img)
                   (sxml:attr element 'src))
              (sxml:set-attr element
                             (list 'src
                                   (url->string (parse-url (car (string-split (sxml:attr element 'src)))
                                                           base-url))))]
             [else element])
       (map (λ (e) (recursive-url-parse e
                                        base-url))
            (sxml:content element)))
      element))


(define (html-content element
                      base-url)
  (string-trim (apply string-append (map (λ (n) (cond [(sxml:element? n)
                                                       (string-trim (srl:sxml->html-noindent n))]
                                                      [(string? n) (xml-attribute-encode n)]))
                                         (sxml:content (recursive-url-parse element
                                                                            base-url))))))


(define (parse-date-time-child elements)
  (map (λ (element)
         (let ([n (sxml:element-name element)])
           (or (and (member n (list 'img 'area)) (if-attr 'alt element))
               (and (equal? n 'data) (or (if-attr 'value element)
                                         (text-content element
                                                       (string->url ""))))
               (and (equal? n 'abbr) (if-attr 'value element))
               (and (member n (list 'del 'ins 'time)) (if-attr 'datetime element))
               (text-content element
                             (string->url "")))))
       elements))


(define (remove-periods input)
  (list->string (filter (λ (c) (not (equal? #\. c)))
                        (string->list input))))


(define (parse-for-time-with-seconds input)
  (ormap (λ (p) (with-handlers ([exn? (λ (exn) #f)])
                  (parse-time (remove-periods input) p)))
         (list "H:m:sX"
               "H:m:sa"
               "H:m:s")))


(define (parse-for-time-no-seconds input)
  (ormap (λ (p) (with-handlers ([exn? (λ (exn) #f)])
                  (parse-time (remove-periods input) p)))
         (list "H:mX"
               "H:ma"
               "H:m"
               "Ha")))


(define (parse-for-date input)
  (ormap (λ (p) (with-handlers ([exn? (λ (exn) #f)])
                  (parse-date input p)))
         (list "y-M-dd"
               "y-DDD")))


(define (parse-iso8601 input)
  (if (regexp-match #rx".+T.+" input)
      (with-handlers ([exn? (λ (exn) #f)])
        (iso8601->datetime input))
      #f))


(define (parse-date-time values)
  (cond [(ormap parse-iso8601
                values)]
        [(let ([this-date (ormap parse-for-date
                                 values)]
               [this-time-with-seconds (ormap parse-for-time-with-seconds
                                              values)]
               [this-time-no-seconds (ormap parse-for-time-no-seconds
                                            values)])
           ; TODO: [tz (findf parse-for-timezone elements)])
           (cond [(and (date? this-date)
                       (time? this-time-with-seconds))
                  (~t (datetime (->year this-date)
                                (->month this-date)
                                (->day this-date)
                                (->hours this-time-with-seconds)
                                (->minutes this-time-with-seconds)
                                (->seconds this-time-with-seconds)
                                (->nanoseconds this-time-with-seconds))
                      "yyyy-MM-dd HH:mm:ss")]
                 [(and (date? this-date)
                       (time? this-time-no-seconds))
                  (~t (datetime (->year this-date)
                                (->month this-date)
                                (->day this-date)
                                (->hours this-time-no-seconds)
                                (->minutes this-time-no-seconds)
                                (->seconds this-time-no-seconds)
                                (->nanoseconds this-time-no-seconds))
                      "yyyy-MM-dd HH:mm")]
                 [(date? this-date)
                  (~t this-date
                      "yyyy-MM-dd")]
                 [else #f]))]
        [else #f]))


(define (value-class-pattern element
                             base-url
                             #:dt [dt #f]) ; https://microformats.org/wiki/value-class-pattern
  (let ([val-children ((sxpath "//*[contains(concat(' ', normalize-space(@class), ' '), ' value ')]") element)]
        [val-title-children ((sxpath "//*[contains(concat(' ', normalize-space(@class), ' '), ' value-title ')]") element)])
    (cond [(and dt
                (pair? val-children))
           (parse-date-time (parse-date-time-child val-children))]
          [(and dt
                (pair? val-title-children))
           (parse-date-time (map (λ (e) (find-attr 'title e))
                                 val-title-children))]
          [(pair? val-children)
           (apply string-append
                  (map (λ (val-child)
                         (let ([n (sxml:element-name val-child)])
                           (or (and (member n (list 'img 'area)) (if-attr 'alt val-child))
                               (and (equal? n 'data) (or (if-attr 'value val-child)
                                                         (text-content val-child
                                                                       base-url)))
                               (and (equal? n 'abbr) (if-attr 'value val-child))
                               (text-content val-child
                                             base-url))))
                       val-children))]
          [(pair? val-title-children)
           (if-attr 'title (car val-title-children))]
          [else #f])))


(define (property->symbol property)
  (string->symbol (string-join
                   (cdr (string-split property
                                      "-"))
                   "-")))


(define (parse-p-* element
                   class-list
                   base-url)
  (map (λ (class)
         (property 'p
                   (property->symbol class)
                   (flatten (list (let ([n (sxml:element-name element)])
                                    (or (value-class-pattern element
                                                             base-url)
                                        (and (member n (list 'abbr 'link)) (find-attr 'title element))
                                        (and (member n (list 'data 'input)) (find-attr 'value element))
                                        (and (member n (list 'img 'area)) (find-attr 'alt element))
                                        (text-content element
                                                      base-url
                                                      #:expand-imgs #t)))))
                   #f))
       class-list))


(define (parse-u-* element
                   class-list
                   base-url)
  (map (λ (class)
         (property 'u
                   (property->symbol class)
                   (list (let ([value (let ([n (sxml:element-name element)])
                                        (cons (or (and (member n (list 'a 'area 'link))
                                                       (if-attr 'href element))
                                                  (and (equal? n 'img) (if-attr 'src element))
                                                  (and (member n (list 'audio 'video 'source 'iframe)) (if-attr 'src element))
                                                  (and (equal? n 'video) (if-attr 'poster element))
                                                  (and (equal? n 'object) (if-attr 'data element))
                                                  (and (member n (list 'audio 'video 'source 'iframe)) (if-attr 'src element))
                                                  (value-class-pattern element
                                                                       base-url)
                                                  (and (equal? n 'abbr) (if-attr 'title element))
                                                  (and (member n (list 'data 'input)) (if-attr 'value element))
                                                  (text-content element
                                                                base-url))
                                              (and (equal? n 'img) (not (null? (find-attr 'src element))) (if-attr 'alt element))))])
                           (if (cdr value)
                               (hasheq 'value (parse-url (if (pair? (car value))
                                                             (caar value)
                                                             (car value))
                                                         base-url)
                                       'alt (cdr value))
                               (parse-url (if (pair? (car value))
                                              (caar value)
                                              (car value))
                                          base-url))))
                   #f))
       class-list))


(define (parse-dt-* element
                    class-list
                    base-url)
  (map (λ (class)
         (property 'dt
                   (property->symbol class)
                   (flatten (list (let ([n (sxml:element-name element)])
                                    (or (value-class-pattern element
                                                             base-url
                                                             #:dt #t)
                                        (and (member n (list 'time 'ins 'del)) (if-attr 'datetime element))
                                        (and (equal? n 'abbr) (if-attr 'title element))
                                        (and (member n (list 'data 'input)) (if-attr 'value element))
                                        (text-content element
                                                      base-url)))))
                   #f))
       class-list))


(define (parse-e-* element
                   class-list
                   base-url)
  (map (λ (class)
         (property 'e
                   (property->symbol class)
                   (list (make-hasheq (list (cons 'value
                                                  (text-content element
                                                                base-url
                                                                #:expand-imgs #t))
                                            (cons 'html
                                                  (html-content element
                                                                base-url)))))
                   #f)) 
       class-list))

(define (parse-properties element
                          base-url)
  (apply append
         (map (λ (pf)
                ((car pf) element
                    (find-class (string-append "^"
                                               (cdr pf)
                                               "(-[a-z0-9]+)?(-[a-z]+)+$")
                                element)
                    base-url))
              (list (cons parse-p-* "p")
                    (cons parse-u-* "u")
                    (cons parse-dt-* "dt")
                    (cons parse-e-* "e")))))


(define (only-of-type type
                      element)
  (if element
      (let ([children-of-type (filter (λ (e) (equal? (sxml:element-name e) type))
                                      (sxml:child-elements element))])
        (if (and (pair? children-of-type)
                 (null? (cdr children-of-type)))
            (car children-of-type)
            #f))
      #f))


(define (imply-properties element
                          mf
                          base-url
                          in-h-*)
  (let ([no-nested (not (or (pair? (microformat-children mf))
                            (findf microformat?
                                   (flatten (map property-value
                                                 (microformat-properties mf))))))]
        [only-child (find-only-child element)]
        [props (parse-properties element
                                 base-url)])
    (microformat (microformat-id mf)
                 (microformat-types mf)
                 (append (if (and (not (findf (λ (p)
                                                (or (equal? (property-title p) 'name)
                                                    (member (property-prefix p) (list 'p 'e))))
                                              (microformat-properties mf)))
                                  no-nested)
                             (list (property 'h
                                             'name
                                             (list (let ([n (sxml:element-name element)])
                                                     (or (and (member n (list 'img 'area)) (if-attr 'alt element))
                                                         (and (equal? n 'abbr) (if-attr 'title element))
                                                         (and (equal? (sxml:element-name only-child) 'img) (if-attr 'alt only-child #:noblank #t))
                                                         (and (equal? (sxml:element-name only-child) 'area) (if-attr 'alt only-child #:noblank #t))
                                                         (and (equal? (sxml:element-name only-child) 'abbr) (if-attr 'title only-child #:noblank #t))
                                                         (and (equal? (sxml:element-name (find-only-child only-child)) 'img) (if-attr 'alt (find-only-child only-child) #:noblank #t))
                                                         (and (equal? (sxml:element-name (find-only-child only-child)) 'area) (if-attr 'alt (find-only-child only-child) #:noblank #t))
                                                         (and (equal? (sxml:element-name (find-only-child only-child)) 'abbr) (if-attr 'title (find-only-child only-child) #:noblank #t))
                                                         (text-content element
                                                                       base-url))))
                                             #f))
                             null)
                         (if (and (not (findf (λ (p)
                                                (or (equal? (property-title p) 'photo)
                                                    (equal? (property-prefix p) 'u)))
                                              (microformat-properties mf)))
                                  no-nested)
                             (let ([implied-photo
                                    (let ([n (sxml:element-name element)])
                                      (or (and (equal? n 'img) (cons (if-attr 'src element)
                                                                     (if-attr 'alt element)))
                                          (and (equal? n 'object) (cons (if-attr 'data element) #f))
                                          (and (only-of-type 'img element) (cons (if-attr 'src (only-of-type 'img element))
                                                                                 (if-attr 'alt (only-of-type 'img element))))
                                          (and (only-of-type 'object element) (cons (if-attr 'data (only-of-type 'object element)) #f))
                                          (and (only-of-type 'img only-child) (cons (if-attr 'src (only-of-type 'img only-child))
                                                                                    (if-attr 'alt (only-of-type 'img only-child))))
                                          (and (only-of-type 'object only-child) (cons (if-attr 'data (only-of-type 'object only-child)) #f))
                                          ))])
                               (if implied-photo
                                   (list (property 'u
                                                   'photo
                                                   (list (if (cdr implied-photo)
                                                             (hasheq 'value (parse-url (car implied-photo)
                                                                                       base-url)
                                                                     'alt (cdr implied-photo))
                                                             (parse-url (car implied-photo)
                                                                        base-url)))
                                                   #f))
                                   null))
                             null)
                         (if (and (not (findf (λ (p)
                                                (or (equal? (property-title p) 'url)
                                                    (equal? (property-prefix p) 'u)))
                                              (microformat-properties mf)))
                                  no-nested)
                             (let ([implied-url (let ([n (sxml:element-name element)])
                                                  (or (and (member n (list 'a 'area)) (if-attr 'href element))
                                                      (and (only-of-type 'a element) (if-attr 'href (only-of-type 'a element)))
                                                      (and (only-of-type 'area element) (if-attr 'href (only-of-type 'area element)))
                                                      (and (only-of-type 'a only-child) (if-attr 'href (only-of-type 'a only-child)))
                                                      (and (only-of-type 'area only-child) (if-attr 'href (only-of-type 'area only-child)))
                                                      ))])
                               (if implied-url
                                   (list (property 'u
                                                   'url
                                                   (list (parse-url implied-url
                                                                    base-url))
                                                   #f))
                                   null))
                             null)
                         (microformat-properties mf))
                 (let ([p-names (if (findf (λ (p) (equal? (property-prefix p) 'p))
                                           props)
                                    (filter (λ (p) (and (equal? (property-prefix p) 'p)
                                                        (equal? (property-title p) 'name)))
                                            (microformat-properties mf))
                                    null)]
                       [e-dicts (filter (λ (p) (and (equal? (property-prefix p) 'e)
                                                    (property-value p)))
                                        props)]
                       [u-urls (if (findf (λ (p) (equal? (property-prefix p) 'u))
                                          props)
                                   (filter (λ (p) (and (equal? (property-prefix p)
                                                               'u)
                                                       (equal? (property-title p)
                                                               'url)))
                                           (microformat-properties mf))
                                   null)]
                       [other-valid-props (filter (λ (p) (or (equal? (property-prefix p) 'p)
                                                             (equal? (property-prefix p) 'u)
                                                             (equal? (property-prefix p) 'dt)))
                                                  props)])
                   (if (not (null? props))
                       (let ([implied-value (append p-names
                                                    e-dicts
                                                    u-urls
                                                    other-valid-props)])
                         (if (null? implied-value)
                             #f
                             (car implied-value)))
                       #f))
                 (microformat-children mf)
                 (microformat-experimental mf))))


(define (process-duplicates elements)
  (let* ([properties (filter property? elements)]
         [duplicate (check-duplicates properties
                                      #:key property-title)])
    (append (if duplicate
                (process-duplicates (append (filter (λ (p) (not (and (equal? (property-prefix p)
                                                                             (property-prefix duplicate))
                                                                     (equal? (property-title p)
                                                                             (property-title duplicate)))))
                                                    properties)
                                            (list (property (property-prefix duplicate)
                                                            (property-title duplicate)
                                                            (foldr append
                                                                   '()
                                                                   (map property-value
                                                                        (filter (λ (p)
                                                                                  (and (equal? (property-prefix p)
                                                                                               (property-prefix duplicate))
                                                                                       (equal? (property-title p)
                                                                                               (property-title duplicate))))
                                                                                properties)))
                                                            (property-experimental duplicate)))))
                properties)
            (filter (λ (e) (not (property? e))) elements))))


(define (recursive-parse elements
                         base-url
                         in-h-*)
  (map (λ (element)
         (let* ([h-types (find-h-types element)]
                [this-id (if-attr 'id
                                  element
                                  #:noblank #t)]
                [parsed-children (flatten (recursive-parse (sxml:child-elements element)
                                                           base-url
                                                           (pair? h-types)))]
                [properties (parse-properties element
                                              base-url)])
           (cond [(and (pair? h-types)
                       (pair? properties))
                  (property 'h
                            (property-title (car properties))
                            (list (imply-properties element
                                                    (microformat this-id
                                                                 h-types
                                                                 (process-duplicates (filter property?
                                                                                             parsed-children))
                                                                 #f
                                                                 (filter microformat? parsed-children)
                                                                 #f)
                                                    base-url
                                                    in-h-*))
                            #f)]
                 [(pair? h-types)
                  (imply-properties element
                                    (microformat this-id
                                                 h-types
                                                 (process-duplicates (filter property?
                                                                             parsed-children))
                                                 #f
                                                 (filter microformat? parsed-children)
                                                 #f)
                                    base-url
                                    in-h-*)]
                 [else (append properties
                               parsed-children)])))
       elements))


(define (parse-elements element-list
                        base-url)
  (filter microformat?
          (flatten (recursive-parse element-list
                                    base-url
                                    #f))))


(define (parse-href element
                    base-url)
  (url->string
   (if (null? (find-attr 'href element))
       base-url
       (let ([href (find-attr 'href element)])
         (parse-url (find-attr 'href element)
                    base-url)))))


(define (element->rels element
                       base-url)
  (let ([href (parse-href element
                          base-url)]
        [rels (if (null? (find-attr 'rel element))
                  null
                  (string-split (find-attr 'rel element)))])
    (cons (if (null? rels)
              (hasheq)
              (make-immutable-hasheq (map (λ (attr) (cons (string->symbol attr)
                                                          (list href)))
                                          rels)))
          (cons (string->symbol href)
                (make-hasheq (filter pair? (append (list (cons 'rels
                                                               rels))
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
                                                               (text-content element
                                                                             base-url
                                                                             #:notrim #t))))))))))


(define (determine-base-url input-doc
                            base-url)
  (let ([base ((sxpath "//base[@href and string-length(normalize-space(@href))]") input-doc)])
    (if (null? base)
        base-url
        (let ([href (find-attr 'href (car base))])
          (if (is-valid-url? href)
              (string->url href)
              (combine-url/relative base-url
                                    href))))))


(define (string->microformats input
                              base-url)
  (let* ([input-doc ((sxml:modify (list "//template" 'delete))
                     (html->xexp input))]
         [rel-pairs (map (λ (e) (element->rels e
                                               (determine-base-url input-doc
                                                                   base-url)))
                         ((sxpath "//a[@rel]|//link[@rel]|//area[@rel]")
                          input-doc))])
    (make-hasheq (list (cons 'items
                             (map microformat->jsexpr
                                  (parse-elements (sxml:child-elements input-doc)
                                                  (determine-base-url input-doc
                                                                      base-url))))
                       (cons 'rels
                             (if (pair? rel-pairs)
                                 (apply hash-union
                                        (map car
                                             rel-pairs)
                                        #:combine/key (lambda (k v1 v2)(remove-duplicates (append v1 v2))))
                                 (hasheq)))
                       (cons 'rel-urls
                             (make-hasheq (reverse (map cdr rel-pairs))))))))