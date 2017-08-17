;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: ASH; Base: 10; -*-

(defpackage #:ash
  (:use #:cl
        #:yason
        #:parenscript)
  
  (:export *ash-host*
           *ash-port*
           #:make-raw-request
           #:make-request

           #:get-sessions
           #:close-session
           #:with-session
           #:safe-with-session
           #:with-a-session

           #:*body-element*
           #:with-body-element
           
           #:close-window
           #:page-open
           #:submit-form
           
           #:page-title

           #:find-elements
           #:find-elements-from
           #:find-element-from
           #:find-element
           #:find-element-by-text
           #:find-elements-by-text
           
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
           #:take-screenshot
           #:execute-javascript)
  
  (:import-from :alexandria :hash-table-keys :alist-hash-table)
  (:shadowing-import-from #:yason #:false))
