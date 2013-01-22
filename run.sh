#!/bin/sh

sbcl --load "article-template.lisp" --eval "(cw:run-web-server)"

