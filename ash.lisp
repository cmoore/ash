
(defpackage #:ash
  (:use #:cl
        #:parenscript)

  (:export *host*
           *port*
           *capabilities*

           #:make-request

           #:get-sessions
           #:close-session
           #:with-session
           #:new-session
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
           #:execute-javascript

           :get-cookies
           :get-cookie
           :set-cookie
           :delete-cookie
           :delete-all-cookies)

  (:import-from :alexandria :alist-hash-table :with-gensyms :if-let))

(in-package :ash)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (push (cons "application" "json") drakma:*text-content-types*))

(defvar *session-id* nil)

(defparameter *host* "127.0.0.1")

(defparameter *port* 4444)

(defparameter *capabilities* nil)

(defun default-capabilities ()
  (jsown:to-json
   `(:obj ("desiredCapabilities"
           . (:obj ("browserName" . "firefox")
                   ("moz:firefoxOptions"
                    . (:obj ("prefs"
                             . (:obj ("dom.ipc.processCount" . 8)
                                     ("devtools.console.stdout.content" . "info")
                                     ("permissions.default.image" . 2)
                                     ("browser.sessionhistory.max_entries" . 10))))))))))


;;
;; Connecting
;;

(defmacro make-request (path &key (method :POST) content parameters)
  (with-gensyms (url)
    `(let ((,url (format nil "http://~a:~a/wd/hub~a" *host* *port* ,path)))
       (jsown:parse
        (drakma:http-request ,url
                             :method ,method
                             :content-type "application/json"
                             :accept "application/json"
                             :external-format-in :utf-8
                             :external-format-out :utf-8
                             :connection-timeout 30
                             ,@(when parameters `(:parameters ,parameters))
                             ,@(when content `(:content ,content)))))))

(defun close-session ()
  "Close the current session, quitting the browser instance."
  (make-request (format nil "/session/~a" *session-id*) :method :delete))

(defmacro with-session ((&key (autoclose t)) &body body)
  "Execute the body in a fresh session and browser instance."
  (let ((response (gensym)))
    `(let ((,response (make-request "/session" :content (default-capabilities))))
       (if-let ((session-id (jsown:val ,response "sessionId")))
         (let ((*session-id* session-id))
           (unwind-protect
                (progn ,@body)
             (when ,autoclose
               (close-window)
               (close-session))))))))

(defun get-sessions ()
  "Returns a list of the session ids currently active on the server"
  (jsown:filter (make-request "/sessions" :method :get)
                "value" map "id"))


;;
;; Navigation
;;

(defun page-open (url)
  "Open a URL"
  (jsown:val (make-request (format nil "/session/~a/url" *session-id*)
                           :content (jsown:to-json `(:obj ("url" . ,url))))
             "state"))

(defun submit-form (element)
  "Assumes the element passed is a FORM element, and submits it."
  (make-request (format nil "/session/~a/element/~a/submit" *session-id* element)))

(defun page-click (element)
  (make-request (format nil "/session/~a/element/~a/click" *session-id* element)))

(defun close-window ()
  "Close the current browser window."
  (make-request (format nil "/session/~a/window" *session-id*)
                :method :delete))


;;
;; Selection
;;

(defun element (bundle)
  (cdadr (jsown:val bundle "value")))

(defun elements (bundle)
  (mapcar #'cdadr (jsown:val bundle "value")))

(defun value (bundle)
  (jsown:val bundle "value"))

(defun find-element (method value)
  "Find the first matching element."
  (let ((result (make-request (format nil "/session/~a/element" *session-id*)
                              :method :POST
                              :content (jsown:to-json
                                        `(:obj ("using" . ,method)
                                               ("value" . ,value))))))
    (values (element result)
            result)))

(defun find-element-from (method value element)
  "Find a single element underneath <element> in the document hierarchy."
  (let ((result (make-request (format nil "/session/~a/element/~a/element" *session-id* element)
                                :method :POST
                                :content (jsown:to-json
                                          `(:obj ("using" . ,method)
                                                 ("value" . ,value))))))
    (values (element result) result)))


(defun find-elements (method value)
  "Find a list of elements."
  (let ((result (make-request (format nil "/session/~a/elements"
                                      *session-id*)
                              :method :post
                              :content (jsown:to-json
                                        `(:obj ("using" . ,method)
                                               ("value" . ,value))))))
    (values (elements result) result)))

(defun find-elements-from (method value element)
  "Find a list of elements underneath <element> in the document hierarchy."
  (let ((result (elements (make-request (format nil "/session/~a/element/~a/elements" *session-id* element)
                                         :method :POST
                                         :content (jsown:to-json
                                                   `(:obj ("using" . ,method)
                                                          ("value" . ,value)))))))
    (values (elements result) result)))



(defun get-current-element ()
  "Returns the 'active' element."
  (element (make-request (format nil "/session/~a/element/active" *session-id*))))

(defun get-element-name (element)
  "Returns the name of the element, ie. FORM, HR, etc."
  (jsown:val (make-request (format nil "/session/~a/element/~a/name" *session-id* element)
                           :method :get)
             "value"))

(defun get-element-text (element)
  "Returns the text, if any, of the element."
  (value (make-request (format nil "/session/~a/element/~a/text" *session-id* element)
                           :method :get)))

(defun get-parent (element)
  "Returns the parent element of <element>."
  (find-element-from "xpath" "parent::*" element))

(defun get-element-attribute (attribute element)
  "Get an attribute from <element>.  SRC, CLASS, etc."
  (value (make-request (format nil "/session/~a/element/~a/attribute/~a"
                               *session-id* element attribute)
                       :method :GET)))

(defun page-title ()
  "Returns the page title."
  (value (make-request (format nil "/session/~a/title" *session-id*)
                      :method :GET)))

(defun get-page-source ()
  "Returns the source of the current page."
  (value (make-request (format nil "/session/~a/source" *session-id*)
                       :method :GET)))

(defun get-element-location (element)
  (value (make-request (format nil "/session/~a/element/~a/location" *session-id* element)
                       :method :get)))

(defun find-element-by-text (tag text)
  (find-element "xpath" (format nil "//~a[normalize-space(.)='~a']" tag text)))

(defun find-elements-by-text (tag text)
  (find-elements "xpath" (format nil "//~a[normalize-space(.)='~a']" tag text)))



;;
;; Cookies
;;

(defun get-cookies ()
  (value (make-request (format nil "/session/~a/cookie" *session-id*)
                       :method :get)))

(defun get-cookie (cookie-name)
  (value (make-request (format nil "/session/~a/cookie/~a"
                               *session-id* cookie-name)
                       :method :get)))

(defun set-cookie (&key name value (path "/") domain (secure-only t) (http-only nil)
                   expiry-time (same-site t))
  (make-request (format nil "/session/~a/cookie" *session-id*)
                :method :post
                :content (jsown:to-json
                          `(:obj ("name" . ,name)
                                 ("value" . ,value)
                                 ("path" . ,path)
                                 ("secure-only" . ,secure-only)
                                 ("http-only" . ,http-only)
                                 ("expiry-time" . ,expiry-time)
                                 ("same-site" . ,same-site)))))

(defun delete-cookie (name)
  (make-request (format nil "/session/~a/cookie/~a"
                        *session-id* name)
                :method :delete))

(defun delete-all-cookies ()
  (make-request (format nil "/session/~a/cookie" *session-id*)
                :method :delete))


;;
;; Misc.
;;


(defun get-log-types ()
  (make-request (format nil "/session/~a/log/types" *session-id*)
                :method :get))

(defun get-log (log-type)
  "Get the logs?"
  (make-request (format nil "/session/~a/log" *session-id*)
                :content (jsown:to-json `(:obj ("type" . ,log-type)))))


(defun send-keys (element text)
  "Send keystrokes to <element>."
  (labels ((string-arrayify (string)
             (map 'list (lambda (x)
                          (format nil "~a" x))
                  string)))
    (make-request (format nil "/session/~a/element/~a/value" *session-id* element)
                  :content (jsown:to-json
                            `(:obj ("value" . ,(string-arrayify text)))))))

(defun page-url ()
  (value (make-request (format nil "/session/~a/url" *session-id*)
                     :method :GET)))

(defun focus-frame (&key (element nil))
  (make-request (format nil "/session/~a/frame" *session-id*)
                :content element))

(defun page-scroll ()
  (make-request (format nil "/session/~a/touch/scroll" *session-id*)))

(defun take-screenshot ()
  (value (make-request (format nil "/session/~a/screenshot" *session-id*)
                      :method :get)))

(defun take-element-screenshot (element)
  (value (make-request (format nil "/session/~a/element/~a/screenshot"
                                 *session-id* element))))

(defun execute-javascript (javascript)
  (make-request (format nil "/session/~a/execute/sync" *session-id*)
                :method :post
                :content (jsown:to-json
                          `(:obj ("script" . ,javascript)
                                 ("args" . "[]")))))


;; Page up and down are broken
;; on lispworks and allegro until
;; I figure out the 'right way' to
;; do it on those platforms.

(defparameter *body-element* nil)

(defmacro with-body-element (&rest body)
  `(let ((*body-element* (find-element "xpath" "//body")))
     ,@body))

(defun page-page-down ()
  (with-body-element
      (send-keys *body-element* "?")))

(defun page-page-up ()
  (with-body-element
      (send-keys *body-element* "?")))





;;
;; Shamelessly stolen^Wborrowed from https://raw.githubusercontent.com/TatriX/cl-selenium-webdriver/master/src/keys.lisp
;;
;;

#-(or lispworks allegro)
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
#-(or lispworks allegro)
(defun key (key)
  (make-string 1 :initial-element (getf *keymap* key)))
