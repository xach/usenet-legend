;;;; package.lisp

(defpackage #:usenet-legend
  (:use #:cl)
  ;; General
  (:export #:id
           #:storage-pathname
           )
  ;; Splitting an archive file
  (:export #:header-matcher
           #:split-archive)
  ;; Corpus-related stuff
  (:export #:corpus
           #:load-corpus
           #:add-article)
  ;; search
  (:export #:search-in-corpus
           #:search-result-page
           #:article-ids)
  ;; Articles
  (:export #:article
           #:message-id
           #:header
           #:date))

(in-package #:usenet-legend)

