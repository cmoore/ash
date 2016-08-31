;; -*- mode: Lisp; Syntax: COMMON-LISP; Package: ASH; Base: 10; -*-

(in-package :ash)

(defvar *session-id* nil)

(defun default-capabilities ()
  (to-json
   (new-js
     ("desiredCapabilities" (new-js ("browserName" "phantomjs"))))))

(defmacro make-raw-request (path &key (method :POST) (content nil))
  `(flexi-streams:octets-to-string
    (drakma:http-request (format nil "http://127.0.0.1:4444/wd/hub~a" ,path)
                         :method ,method
                         :content-type "application/json"
                         :accept "application/json"
                         :external-format-in :utf-8
                         :external-format-out :utf-8
                         ,@(when content `(:content ,content)))))

(defmacro make-request (path &key (method :POST) (content nil))
  `(parse
    (flexi-streams:octets-to-string
     (drakma:http-request (format nil "http://127.0.0.1:4444/wd/hub~a" ,path)
                          :method ,method
                          :content-type "application/json"
                          :accept "application/json"
                          :external-format-in :utf-8
                          :external-format-out :utf-8
                          ,@(when content `(:content ,content))))))

(defun close-session ()
  (make-request "/session/~a" :method :delete))

(defmacro with-session ((&key (autoclose nil))  &body body)
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
        (val result "state"))))

(defun get-element-name (element)
  (make-request (format nil "/session/~a/element/~a/name" *session-id* element)))

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
  (make-request (format nil "/session/~a/screenshot" *session-id*)))

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
