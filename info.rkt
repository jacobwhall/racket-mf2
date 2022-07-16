#lang info

(define collection "microformats")

(define pkg-desc "A microformats2 parser for Racket")

(define version "0.0.1")

(define pkg-authors '(jacob))

(define license '(CC0))

(define deps '("base"
	       "sxml"
	       "gregor"
	       "html-parsing"
	       "rackunit-lib"))

(define build-deps '("scribble-lib"
		     "racket-doc"
		     "rackunit-lib"))

(define scribblings '(("docs/microformats.scrbl"
		       ())))
