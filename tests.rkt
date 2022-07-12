#lang racket

(require json
         net/url
         rackunit
         "main.rkt")


(define (compare-output input-html-path
                        input-json-path)
  (let ([html-jsexpr (string->microformats (file->string input-html-path)
                                           (url "http" #f "example.com" #f #t (list (path/param "" '())) '() #f))]
        [json-jsexpr (string->jsexpr (file->string input-json-path))])
    (check-equal? (jsexpr->string html-jsexpr)
                  (jsexpr->string json-jsexpr)
                  (string-append "Offending test is in "
                                 (path->string input-html-path)))))


(define (remove-extension path)
  (let ([path-string (path->string path)])
    (let ([n (string-length path-string)])
      (substring path-string
                 0
                 (- n 5)))))


(define (check-and-recurse files)
  (compare-output (first files)
                   (second files))
  (if (> (length files) 2)
      (recursive-test (cdr files))
      "There are no more files to compare."))


(define (recursive-test files)
  (if (> (length files) 1)
      (if (equal? (remove-extension (file-name-from-path (first files)))
                  (remove-extension (file-name-from-path (second files))))
          (check-and-recurse files)
          (recursive-test (cdr files)))
   "There is only one file left, but nothing to compare it to!"))

(recursive-test (flatten (map (λ (d) (directory-list d
                                                     #:build? #t))
                              (directory-list (build-path (current-directory)
                                                          "tests/tests/microformats-v2")
                                              #:build? #t))))
