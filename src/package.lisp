;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: ASH; Base: 10; -*-

(defpackage #:ash
  (:use #:cl
        #:jsown
        #:parenscript)
  
  (:export #:make-raw-request
           #:make-request

           #:get-sessions
           #:close-session
           #:with-session
           #:with-a-session
           
           #:close-window
           #:page-open
           #:submit-form
           
           #:page-title
           
           #:find-elements-from
           #:find-element-from
           #:find-element

           #:get-current-element
           #:get-parent
           #:get-element-attribute
           #:get-element-name
           
           #:send-keys
           #:key

           #:page-open
           
           #:page-source
           #:page-click
           #:page-url
           #:focus-frame
           #:page-scroll
           #:take-screenshot))
