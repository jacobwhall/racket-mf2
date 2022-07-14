#lang scribble/manual
@require[@for-label[microformats
                    racket/base]]

@title{microformats}
@author{Jacob Hall}

@defmodule[microformats]

microformats defines expressions to describe microformats2. It exposes a parsing function that makes it easy to consume microformats.

This package is a work in progress, and its API will almost definitely change.

@section{Getting Started}

@racketblock[
(require microformats
         net/url)
(jsexpr->string (string->microformats "<a class=\"h-card\" href=\"http://benward.me/\">Ben Ward</a>"
                                      (string->url "http://example.com")))
]
