;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

(defpackage #:ash
  (:use #:cl
        #:yason
        #:parenscript)
  
  (:export *ash-host*
           *ash-port*
           #:make-request

           #:get-sessions
           #:close-session
           #:with-session
           #:safe-with-session
           #:with-a-session
           #:new-session
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
           
           #:get-page-source
           #:page-click
           #:page-url
           #:focus-frame
           #:page-scroll
           #:take-screenshot
           #:execute-javascript)
  
  (:local-nicknames (:alex :alexandria))
  (:shadowing-import-from #:yason #:false))

(in-package :ash)

(defmacro sassoc (place alist)
  `(assoc ,place ,alist :test #'string=))

(defvar *session-id* nil)

(defparameter *ash-host* "127.0.0.1")

(defparameter *ash-port* 4444)

(defmacro make-request (path &key (method :POST) (content nil) (parameters nil))
  `(let ((yason:*parse-object-as* :alist))
     (yason:parse
      (drakma:http-request (format nil "http://~a:~a/wd/hub~a" *ash-host* *ash-port* ,path)
                           :method ,method
                           :content-type "application/json"
                           :accept "application/json"
                           :external-format-in :utf-8
                           :external-format-out :utf-8
                           :connection-timeout 60
                           ,@(when parameters `(:parameters ,parameters))
                           ,@(when content `(:content ,content))))))

(defun default-capabilities ()
  (with-output-to-string (sink)
    (yason:encode
     (alex:alist-hash-table
      (list (cons "desiredCapabilities"
                  (alex:alist-hash-table
                   (list (cons "browserName" "chrome")
                         ;; (cons "goog:chromeOptions" (alist-hash-table
                         ;;                             (list (cons ""))))
                         ;;(cons "webdriver.chrome.driver" "/usr/local/bin/chromedriver76")
                         (cons "goog:loggingPrefs" (alex:alist-hash-table
                                                    (list (cons "browser" "ALL")
                                                          (cons "server" "ALL")
                                                          (cons "performance" "ALL")
                                                          (cons "profiler" "ALL")
                                                          (cons "client" "ALL")
                                                          (cons "driver" "INFO"))))
                         (cons "moz:firefoxOptions"
                               (alex:alist-hash-table
                                (list
                                 ;;(cons "binary" "/usr/local/bin/firefox")
                                 (cons "prefs"
                                       (alex:alist-hash-table
                                        (list (cons "dom.ipc.processCount" 8)
                                              (cons "devtools.console.stdout.content" "info")
                                              (cons "permissions.default.image" 2)
                                              (cons "browser.sessionhistory.max_entries" 10))))))))))))
     sink)))

(defun to-json (list)
  (check-type list list)
  (with-output-to-string (sink)
    (yason:encode (alex:alist-hash-table list) sink)))

(defun close-session ()
  "Close the current session, quitting the browser instance."
  (make-request (format nil "/session/~a" *session-id*) :method :delete))


(defmacro with-session-id (session-id &rest body)
  `(let ((*session-id* ,session-id))
     (progn ,@body)))


(defmacro with-session ((&key (autoclose t)) &body body)
  "Execute the body in a fresh session and browser instance."
  (alex:with-gensyms (result-alist result)
    `(let ((,result-alist (make-request "/session" :content (default-capabilities))))
       (alexandria:if-let ((session-id (cdr (sassoc "sessionId" ,result-alist))))
         (let ((*session-id* session-id))
           (let ((,result (progn ,@body)))
             ,(when autoclose
                `(progn
                   (close-window)
                   (close-session)))
             ,result))
         (list `(cons "error" . ,(assoc :value ,result-alist)))))))

(defun new-session ()
  (cdr (sassoc "webdriver.remote.sessionid"
               (cdr (sassoc "value" (make-request "/session" :content (default-capabilities)))))))

(defmacro with-a-session (&body body)
  "Execute the body using the first avilable session on the server."
  (let ((gs-sessions (gensym)))
    `(let ((,gs-sessions (get-sessions)))
       (optima:match ,gs-sessions
         (() (with-session (:autoclose nil)
               ,@body))
         (otherwise (let ((*session-id* (car ,gs-sessions)))
                      ,@body))))))


(defmacro with-body-element (&rest body)
  `(let ((*body-element* (find-element "xpath" "//body")))
     ,@body))


(defun get-sessions ()
  "Returns a list of the session ids currently active on the server."
  (let ((lx (make-request "/sessions" :method :GET)))
    (mapcar (lambda (x)
              (cdr (sassoc "id" x)))
            (cdr (sassoc "value" lx)))))

(defun close-window ()
  "Close the current browser window."
  (make-request (format nil "/session/~a/window" *session-id*)
                :method :DELETE))

(defun get-log-types ()
  (make-request (format nil "/session/~a/log/types" *session-id*)
                :method :get))

(defun get-log (log-type)
  "Get the logs?"
  (make-request (format nil "/session/~a/log" *session-id*)
                :content (to-json (list (cons "type" log-type)))))

(defun page-open (url)
  "Open a URL"
  (make-request (format nil "/session/~a/url" *session-id*)
                :content (to-json (list (cons "url" url)))))

(defun submit-form (element)
  "Assumes the element passed is a FORM element, and submits it."
  (make-request (format nil "/session/~a/element/~a/submit" *session-id* element)))

(defun page-title ()
  "Returns the page title."
  (make-request (format nil "/session/~a/title" *session-id*)
                   :method :GET))

(defun find-elements (method value)
  "Find a list of elements."
  (let ((result (with-body-element
                    (find-elements-from method value *body-element*))))
    
    (values (mapcar #'cdar (cdr (assoc "value" result :test #'string=)))
            result)))

(defun find-elements-from (method value element)
  "Find a list of elements underneath <element> in the document hierarchy."
  (make-request (format nil "/session/~a/element/~a/elements" *session-id* element)
                   :method :POST
                   :content (to-json (list (cons "using" method)
                                           (cons "value" value)))))

(defun find-element-from (method value element)
  "Find a single element underneath <element> in the document hierarchy."
  (make-request (format nil "/session/~a/element/~a/element" *session-id* element)
                   :method :POST
                   :content (to-json (list (cons "using" method)
                                           (cons "value" value)))))

(defun find-element (method value)
  "Find the first matching element."
  (let ((result (make-request (format nil "/session/~a/element" *session-id*)
                              :method :POST
                              :content (to-json (list (cons "using" method)
                                                      (cons "value" value))))))
    (values (cdadr (assoc "value" result :test #'string=))
            result)))

(defun get-current-element ()
  "Returns the 'active' element."
  (make-request (format nil "/session/~a/element/active" *session-id*)))

(defun get-element-name (element)
  "Returns the name of the element, ie. FORM, HR, etc."
  (make-request (format nil "/session/~a/element/~a/name" *session-id* element)))

(defun get-element-text (element)
  "Returns the text, if any, of the element."
  (let ((result (make-request (format nil "/session/~a/element/~a/text" *session-id* element)
                              :method :GET)))
    
    (values (cdr (assoc "value" result :test #'string=))
            result )))

(defun get-parent (element)
  "Returns the parent element of <element>."
  (find-element-from "xpath" "parent::*" element))

(defun get-element-attribute (attribute element)
  "Get an attribute from <element>.  SRC, CLASS, etc."
  (make-request (format nil "/session/~a/element/~a/attribute/~a" *session-id* element attribute)
                   :method :GET))

(defun send-keys (element text)
  "Send keystrokes to <element>."
  (labels ((string-arrayify (string) (map 'list (lambda (x)
                                                  (format nil "~a" x))
                                          string)))

    (make-request (format nil "/session/~a/element/~a/value" *session-id* element)
                  :content (to-json (list (cons "value" (string-arrayify text)))))))

(defun get-page-source ()
  "Returns the source of the current page."
  (make-request (format nil "/session/~a/source" *session-id*)
                :method :GET))

(defun get-element-location (element)
  (make-request (format nil "/session/~a/element/~a/location" *session-id* element)))

(defun page-click (element)
  (cdr
   (sassoc "state"
           (make-request (format nil "/session/~a/element/~a/click" *session-id* element)))))

(defun page-url ()
  (cdr (assoc "value"
              (make-request (format nil "/session/~a/url" *session-id*)
                            :method :GET)
              :test #'string=)))

(defun focus-frame (&key (element nil))
  (make-request (format nil "/session/~a/frame" *session-id*)
                :content element))

(defun page-scroll ()
  (make-request (format nil "/session/~a/touch/scroll" *session-id*)))

(defun take-screenshot ()
  (make-request (format nil "/session/~a/screenshot" *session-id*)
                :method :get))

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

#-allegro
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
#-allegro
(defun key (key)
  (make-string 1 :initial-element (getf *keymap* key)))
