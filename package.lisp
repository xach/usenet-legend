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
           #:first-article
           #:last-article
           #:next-article
           #:previous-article
           #:add-article)
  ;; search
  (:export #:search-in-corpus
           #:search-result-page
           #:article-ids)
  ;; Articles
  (:export #:article
           #:message-id
           #:header
           #:newsgroups
           #:from
           #:date))

(in-package #:usenet-legend)

