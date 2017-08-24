;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

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

(in-package :ash)

(defvar *session-id* nil)

(defparameter *ash-host* "127.0.0.1")

(defparameter *ash-port* 4444)

(defmacro make-request (path &key (method :POST) (content nil) (parameters nil))
  `(yason:parse
    (flexi-streams:octets-to-string 
     (drakma:http-request (format nil "http://~a:~a/wd/hub~a" *ash-host* *ash-port* ,path)
                          :method ,method
                          :content-type "application/json"
                          :accept "application/json"
                          :external-format-in :utf-8
                          :external-format-out :utf-8
                          :connection-timeout 60
                          ,@(when parameters `(:parameters ,parameters))
                          ,@(when content `(:content ,content))))))

(defun agents ()
  (list "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Ubuntu/11.04 Chromium/17.0.963.56 Chrome/17.0.963.56 Safari/535.11"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11"
        "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.12 Safari/535.11"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.8 (KHTML, like Gecko) Chrome/17.0.940.0 Safari/535.8"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.77 Safari/535.7ad-imcjapan-syosyaman-xkgi3lqg03!wgz"
        "Mozilla/5.0 (X11; CrOS i686 1193.158.0) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7"
        "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7"
        "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.7xs5D9rRDFpg2g"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.8 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.8"
        "Mozilla/5.0 (Windows NT 5.2; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.7"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7"
        "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.6 (KHTML, like Gecko) Chrome/16.0.897.0 Safari/535.6"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.874.54 Safari/535.2"
        "Mozilla/5.0 (X11; FreeBSD i386) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.874.121 Safari/535.2"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.2 (KHTML, like Gecko) Ubuntu/11.10 Chromium/15.0.874.120 Chrome/15.0.874.120 Safari/535.2"
        "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.874.120 Safari/535.2"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.872.0 Safari/535.2"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.2 (KHTML, like Gecko) Ubuntu/11.04 Chromium/15.0.871.0 Chrome/15.0.871.0 Safari/535.2"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.864.0 Safari/535.2"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.861.0 Safari/535.2"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_0) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.861.0 Safari/535.2"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.861.0 Safari/535.2"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.860.0 Safari/535.2"
        "Chrome/15.0.860.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/533.20.25 (KHTML, like Gecko) Version/15.0.860.0"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.835.186 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.834.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/11.04 Chromium/14.0.825.0 Chrome/14.0.825.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.824.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.815.10913 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.815.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/11.04 Chromium/14.0.814.0 Chrome/14.0.814.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.814.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/10.04 Chromium/14.0.813.0 Chrome/14.0.813.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.813.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.813.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.813.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_7) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.813.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.812.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.811.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.810.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.810.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.809.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/10.10 Chromium/14.0.808.0 Chrome/14.0.808.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/10.04 Chromium/14.0.808.0 Chrome/14.0.808.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/10.04 Chromium/14.0.804.0 Chrome/14.0.804.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/11.04 Chromium/14.0.803.0 Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_7) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_5_8) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.803.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.801.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_5_8) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.801.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.794.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.794.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.792.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.792.0 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.792.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; PPC Mac OS X 10_6_7) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.790.0 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_7) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.790.0 Safari/535.1"
        "Mozilla/5.0 (Windows; U; Windows NT 6.1) AppleWebKit/526.3 (KHTML, like Gecko) Chrome/14.0.564.21 Safari/526.3"
        "Mozilla/5.0 (X11; CrOS i686 13.587.48) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.43 Safari/535.1"
        "Mozilla/5.0 Slackware/13.37 (X11; U; Linux x86_64; en-US) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41"
        "Mozilla/5.0 ArchLinux (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Ubuntu/11.04 Chromium/13.0.782.41 Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.2; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_7) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_3) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_2) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.41 Safari/535.1"))

(defun default-capabilities ()
  (with-output-to-string (sink)
    (yason:encode
     (alist-hash-table
      (list (cons "desiredCapabilities"
                  (alist-hash-table
                   (list (cons "browserName" "firefox")
                         (cons "moz:firefoxOptions" (alist-hash-table
                                                     (list
                                                      (cons "prefs"
                                                            (alist-hash-table
                                                             (list (cons "dom.ipc.processCount" 8)
                                                                   (cons "general.useragent.override" (nth (random (length (agents))) (agents)))
                                                                   (cons "permissions.default.image" 2)
                                                                   (cons "browser.sessionhistory.max_entries" 10))))))))))))
     sink)))

(defun to-json (list)
  (check-type list list)
  (with-output-to-string (sink)
    (yason:encode (alist-hash-table list) sink)))

(defun close-session ()
  "Close the current session, quitting the browser instance."
  (make-request (format nil "/session/~a" *session-id*) :method :delete))

(defmacro with-session ((&key (autoclose t)) &body body)
  "Execute the body in a fresh session and browser instance."
  (let ((json (gensym))
        (result (gensym)))
    `(let ((,json (make-request "/session" :content (default-capabilities))))
       (if (member "sessionId" (hash-table-keys ,json) :test #'string=)
           (let ((*session-id* (gethash "sessionId" ,json)))
             (let ((,result (progn ,@body)))
               ,(when autoclose
                  `(progn
                     (close-window)
                     (close-session)))
               ,result))
           (to-json (list (cons "error" (gethash "value" ,json))))))))

(defmacro safe-with-session (&body body)
  (let ((json (gensym))
        (result (gensym)))
    `(let ((,json (make-request "/session" :content (default-capabilities))))
       (unwind-protect 
            (if (member "sessionId" (hash-table-keys ,json) :test #'string=)
                (let ((*session-id* (gethash "sessionId" ,json)))
                  (let ((,result (progn ,@body)))
                    (close-window)
                    (close-session)
                    ,result))
                (to-json (list (cons "error" (gethash ,json "value")))))
         (progn
           (close-window)
           (close-session))))))

(defmacro with-body-element (&rest body)
  `(let ((*body-element* (find-element "xpath" "//body")))
     ,@body))

(defun get-sessions ()
  "Returns a list of the session ids currently active on the server."
  (let ((lx (make-request "/sessions" :method :GET)))
    (mapcar (lambda (x)
              (gethash "id" x))
            (gethash "value" lx))))

(defmacro with-a-session (&body body)
  "Execute the body using the first avilable session on the server."
  (let ((gs-sessions (gensym)))
    `(let ((,gs-sessions (get-sessions)))
       (optima:match ,gs-sessions
         (() (with-session (:autoclose nil)
               ,@body))
         (otherwise (let ((*session-id* (car ,gs-sessions)))
                      ,@body))))))

(defun close-window ()
  "Close the current browser window."
  (make-request (format nil "/session/~a/window" *session-id*)
                :method :DELETE))

(defun page-open (url)
  "Open a URL"
  (make-request (format nil "/session/~a/url" *session-id*)
                :content (to-json (list (cons "url" url)))))

(defun submit-form (element)
  "Assumes the element passed is a FORM element, and submits it."
  (make-request (format nil "/session/~a/element/~a/submit" *session-id* element)))

(defun page-title ()
  "Returns the page title."
  (let ((result (make-request (format nil "/session/~a/title" *session-id*)
                              :method :GET)))
    (gethash "value" result)))

(defun find-elements (method value)
  "Find a list of elements."
  (with-body-element
    (find-elements-from method value *body-element*)))

(defun find-elements-from (method value element)
  "Find a list of elements underneath <element> in the document hierarchy."
  (let ((result (make-request (format nil "/session/~a/element/~a/elements" *session-id* element)
                              :method :POST
                              :content (to-json (list (cons "using" method)
                                                      (cons "value" value))))))
    (values (mapcar (lambda (x) (gethash "ELEMENT" x))
                    (gethash "value" result)) result)))

(defun find-element-from (method value element)
  "Find a single element underneath <element> in the document hierarchy."
  (let ((result (make-request (format nil "/session/~a/element/~a/element" *session-id* element)
                              :method :POST
                              :content (to-json (list (cons "using" method)
                                                      (cons "value" value))))))
    (when (string= (gethash "state" result) "success")
      (gethash "ELEMENT" (gethash "value" result)))))

(defun find-element (method value)
  "Find the first matching element."
  (let ((result (make-request (format nil "/session/~a/element" *session-id*)
                              :method :POST
                              :content (to-json (list (cons "using" method)
                                                      (cons "value" value))))))
    (values (gethash "ELEMENT" (gethash "value" result)) result)))

(defun get-current-element ()
  "Returns the 'active' element."
  (let ((result (make-request (format nil "/session/~a/element/active" *session-id*))))
    (gethash "ELEMENT" (gethash "value" result))))

(defun get-element-name (element)
  "Returns the name of the element, ie. FORM, HR, etc."
  (make-request (format nil "/session/~a/element/~a/name" *session-id* element)))

(defun get-element-text (element)
  "Returns the text, if any, of the element."
  (let ((result (make-request (format nil "/session/~a/element/~a/text" *session-id* element)
                              :method :GET)))
    (values (gethash "value" result) result)))

(defun get-parent (element)
  "Returns the parent element of <element>."
  (find-element-from "xpath" "parent::*" element))

(defun backwards-find-element-from (method value element predicate)
  "Finds the first matching element above the element specified in <element>."
  (filter predicate (find-elements-from method value
                                        (find-element-from "xpath" ".." element))))

(defun get-element-attribute (attribute element)
  "Get an attribute from <element>.  SRC, CLASS, etc."
  (let ((result (make-request (format nil "/session/~a/element/~a/attribute/~a" *session-id* element attribute)
                              :method :GET)))
    (gethash "value" result)))

(defun send-keys (element text)
  "Send keystrokes to <element>."
  (labels ((string-arrayify (string) (map 'list (lambda (x)
                                                  (format nil "~a" x))
                                          string)))

    (make-request (format nil "/session/~a/element/~a/value" *session-id* element)
                  :content (to-json (list (cons "value" (string-arrayify text)))))))

(defun page-source ()
  "Returns the source of the current page."
  (gethash "value"
           (make-request (format nil "/session/~a/source" *session-id*)
                           :method :GET)))

(defun get-element-location (element)
  (make-request (format nil "/session/~a/element/~a/location" *session-id* element)))

(defun page-click (element)
  (make-request (format nil "/session/~a/element/~a/click" *session-id* element)))

(defun page-url ()
  (gethash "value" (make-request (format nil "/session/~a/url" *session-id*)
                           :method :GET)))

(defun focus-frame (&key (element nil))
  (make-request (format nil "/session/~a/frame" *session-id*)
                :content element))

(defun page-scroll ()
  (make-request (format nil "/session/~a/touch/scroll" *session-id*)))

(defun take-screenshot ()
  (let ((result (make-request (format nil "/session/~a/screenshot" *session-id*)
                              :method :get)))
    (gethash "value" result)))

(defun execute-javascript (javascript)
  (make-request (format nil "/session/~a/execute/sync" *session-id*)
                :method :post
                :content (to-json (list (cons "script" javascript)
                                        (cons "args" (list "a"))))))

(defparameter *body-element* nil)

(defun page-page-down ()
  (with-body-element
      (send-keys *body-element* "?")))

(defun page-page-up ()
  (with-body-element
      (send-keys *body-element* "?")))

(defun find-element-by-text (tag pattern)
  (find-element "xpath" (format nil "//~a[normalize-space(.)='~a']" tag pattern)))

(defun find-elements-by-text (tag pattern)
  (find-elements "xpath" (format nil "//~a[normalize-space(.)='~a']" tag pattern)))



;;
;; Shamelessly stolen^Wborrowed from https://raw.githubusercontent.com/TatriX/cl-selenium-webdriver/master/src/keys.lisp
;;
;;

(defvar *keymap*
  '(:null #\ue000
    :cancel #\ue001
    :help #\ue002
    :backspace #\ue003
    :tab #\ue004
    :clear #\ue005
    :return #\ue006
    :enter #\ue007
    :shift #\ue008
    :control #\ue009
    :alt #\ue00a
    :pause #\ue00b
    :escape #\ue00c
    :space #\ue00d
    :page-up #\ue00e
    :page-down #\ue00f
    :end #\ue010
    :home #\ue011
    :left-arrow #\ue012
    :up-arrow #\ue013
    :right-arrow #\ue014
    :down-arrow #\ue015
    :insert #\ue016
    :delete #\ue017
    :semicolon #\ue018
    :equals #\ue019
    :numpad-0 #\ue01a
    :numpad-1 #\ue01b
    :numpad-2 #\ue01c
    :numpad-3 #\ue01d
    :numpad-4 #\ue01e
    :numpad-5 #\ue01f
    :numpad-6 #\ue020
    :numpad-7 #\ue021
    :numpad-8 #\ue022
    :numpad-9 #\ue023
    :multiply #\ue024
    :add #\ue025
    :separator #\ue026
    :substract #\ue027
    :decimal #\ue028
    :divide #\ue029
    :f1 #\ue031
    :f2 #\ue032
    :f3 #\ue033
    :f4 #\ue034
    :f5 #\ue035
    :f6 #\ue036
    :f7 #\ue037
    :f8 #\ue038
    :f9 #\ue039
    :f10 #\ue03a
    :f11 #\ue03b
    :f12 #\ue03c
    :meta #\ue03d))

;; TODO: prepare all keys on compile
(defun key (key)
  (make-string 1 :initial-element (getf *keymap* key)))
