;; -*- mode: Lisp; Syntax: COMMON-LISP; Base: 10; eval: (hs-hide-all) -*-

(in-package :ash)

(defvar *session-id* nil)

(defparameter *ash-host* "127.0.0.1")
(defparameter *ash-port* 4444)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro make-request (path &key (method :POST) (content nil) (parameters nil))
    `(jsown:parse
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
     (alexandria:alist-hash-table
      (list (cons "desiredCapabilities" (alexandria:alist-hash-table
                                         (list (cons "browserName" "firefox")
                                               (cons "moz:firefoxOptions" (alist-hash-table
                                                                           (list ;;(cons "args" (list "-headless" "-profile" "/home/cmoore/quicklisp/local-projects/lengle/prof/ff"))
                                                                                 (cons "prefs" (alist-hash-table
                                                                                                (list (cons "dom.ipc.processCount" 8)
                                                                                                      (cons "permissions.default.image" 2)
                                                                                                      (cons "browser.sessionhistory.max_entries" 10))))))))))))
     sink)))
;; (to-json
;;  (new-js
;;    ("desiredCapabilities" (new-js ("browserName" "firefox")))))

(defmacro make-raw-request (path &key (method :POST) (content nil))
  `(flexi-streams:octets-to-string
    (drakma:http-request (format nil "http://127.0.0.1:4444/wd/hub~a" ,path)
                         :method ,method
                         :content-type "application/json"
                         :accept "application/json"
                         :external-format-in :utf-8
                         :external-format-out :utf-8
                         ,@(when content `(:content ,content)))))

(defun close-session ()
  (make-request (format nil "/session/~a" *session-id*) :method :delete))

(defmacro with-session ((&key (autoclose t)) &body body)
  (let ((json (gensym))
        (result (gensym)))
    `(let ((,json (make-request "/session" :content (default-capabilities))))
       (if (member "sessionId" (keywords ,json) :test #'string=)
           (let ((*session-id* (val ,json "sessionId")))
             (let ((,result (progn ,@body)))
               ,(when autoclose
                  `(progn
                     (close-window)
                     (close-session)))
               ,result))
           (to-json (new-js ("error" (val ,json "value"))))))))

(defmacro safe-with-session (&body body)
  (let ((json (gensym))
        (result (gensym)))
    `(let ((,json (make-request "/session" :content (default-capabilities))))
       (unwind-protect 
            (if (member "sessionId" (keywords ,json) :test #'string=)
                (let ((*session-id* (val ,json "sessionId")))
                  (let ((,result (progn ,@body)))
                    (close-window)
                    (close-session)
                    ,result))
                (to-json (new-js ("error" (val ,json "value")))))
         (progn
           (close-window)
           (close-session))))))

(defun get-sessions ()
  (let ((lx (make-request "/sessions" :method :GET)))
    (mapcar (lambda (x)
              (val x "id")) (val lx "value"))))

(defmacro with-a-session (&body body)
  (let ((gs-sessions (gensym)))
    `(let ((,gs-sessions (get-sessions)))
       (optima:match ,gs-sessions
         (() (with-session (:autoclose nil)
               ,@body))
         (otherwise (let ((*session-id* (car ,gs-sessions)))
                      ,@body))))))

(defmacro -with-a-session (&body body)
  (let ((current-ids (gensym)))
    `(let* ((,current-ids (get-sessions))
            (*session-id* (optima:match ,current-ids
                            (() (error "No current sessions."))
                            (otherwise (car ,current-ids)))))
       ,@body)))


(defun close-window ()
  (make-request (format nil "/session/~a/window" *session-id*)
                :method :DELETE))

(defun page-open (url)
  (make-request (format nil "/session/~a/url" *session-id*)
                :content (to-json (new-js ("url" url)))))

(defun submit-form (element)
  (make-request (format nil "/session/~a/element/~a/submit" *session-id* element)))

(defun page-title ()
  (val (make-request (format nil "/session/~a/title" *session-id*)
                           :method :GET)
             "value"))

(defun find-elements (method value)
  (let ((body (find-element "tag name" "body")))
    (find-elements-from method value body)))

(defun find-elements-from (method value element)
  (let ((result (make-request (format nil "/session/~a/element/~a/elements" *session-id* element)
                              :method :POST
                              :content (to-json (new-js ("using" method)
                                                        ("value" value))))))
    (if (string= (ignore-errors (val result "state")) "success")
        (mapcar (lambda (e)
                  (val e "ELEMENT"))
                (val result "value"))
        (val result "state"))))

(defun find-element-from (method value element)
  (let ((result (make-request (format nil "/session/~a/element/~a/element" *session-id* element)
                              :method :POST
                              :content (to-json (new-js ("using" method)
                                                        ("value" value))))))
    (if (string= (ignore-errors (val result "state")) "success")
        (val (val result "value") "ELEMENT")
        (val result "state"))))

(defun find-element (method value)
  (let ((result (make-request (format nil "/session/~a/element" *session-id*)
                              :method :POST
                              :content (to-json (new-js ("using" method)
                                                        ("value" value))))))
    (if (string= (ignore-errors (val result "state")) "success")
        (val (val result "value") "ELEMENT")
        (let ((retv (val result "state")))
          (cond ((string= "no such element" retv) nil)
                (t retv)))
        ;(cond ((string= "unknown element" (val result "state"))))
        )))

(defun get-current-element ()
  (jsown:filter (make-request (format nil "/session/~a/element/active" *session-id*))
                "value" "ELEMENT"))

(defun get-element-name (element)
  (make-request (format nil "/session/~a/element/~a/name" *session-id* element)))

(defun get-element-text (element)
  (let ((result (make-request (format nil "/session/~a/element/~a/text" *session-id* element)
                              :method :GET)))
    (values (jsown:filter result "value") result)))

(defun get-parent (element)
  (find-element-from "xpath" "parent::*" element))

(defun backwards-find-element-from (method value element predicate)
  (filter predicate (find-elements-from method value
                                        (find-element-from "xpath" ".." element))))

(defun get-element-attribute (attribute element)
  (let ((result (make-request (format nil "/session/~a/element/~a/attribute/~a" *session-id* element attribute)
                              :method :GET)))
    (if (string= (ignore-errors (val result "state")) "success")
        (val result "value")
        (val result "state"))))

(defun send-keys (element text)
  (labels ((string-arrayify (string) (map 'list (lambda (x)
                                                  (format nil "~a" x))
                                          string)))

    (make-request (format nil "/session/~a/element/~a/value" *session-id* element)
                  :content (to-json (new-js ("value" (string-arrayify text)))))))

(defun page-source ()
  (val (make-request (format nil "/session/~a/source" *session-id*)
                           :method :GET)
             "value"))

(defun get-element-location (element)
  (make-request (format nil "/session/~a/element/~a/location" *session-id* element)))

(defun page-click (element)
  (make-request (format nil "/session/~a/element/~a/click" *session-id* element)))

(defun page-url ()
  (val (make-request (format nil "/session/~a/url" *session-id*)
                           :method :GET)
             "value"))

(defun focus-frame (&key (element nil))
  (make-request (format nil "/session/~a/frame" *session-id*)
                :content element))

(defun page-scroll ()
  (make-request (format nil "/session/~a/touch/scroll" *session-id*)))

(defun take-screenshot ()
  (let ((result (make-request (format nil "/session/~a/screenshot" *session-id*)
                              :method :get)))
    (jsown:val result "value")))

(defun execute-javascript (javascript)
  (make-request (format nil "/session/~a/execute/sync" *session-id*)
                :method :post
                :content (with-output-to-string (stream)
                           (yason:encode-alist (list (cons "script" javascript)
                                                     (cons "args" (list "a")))
                                               stream))))

(defparameter *body-element* nil)

(defmacro with-body-element (&rest body)
  `(let ((*body-element* (find-element "css selector" "body")))
     ,@body))

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
