;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: ASH; Base: 10; -*-

(defpackage #:ash
  (:use #:cl
        #:jsown
        #:parenscript)
  
  (:export *host*
           *port*
           #:make-raw-request
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
           #:find-element-by-text

           #:get-element-location
           #:get-current-element
           #:get-parent
           #:get-element-attribute
           #:get-element-name
           #:get-element-text
           
           #:send-keys
           #:key

           #:page-open
           
           #:page-source
           #:page-click
           #:page-url
           #:focus-frame
           #:page-scroll
           #:take-screenshot))
