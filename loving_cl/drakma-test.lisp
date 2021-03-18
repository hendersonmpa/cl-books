;; pg 96
(ql:quickload :drakma)
(multiple-value-setq
    (data http-response-code headers)
  (drakma:http-request "http://markwatson.com"))

(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(defpackage hdemo
  (:use :cl
        :cl-who
        :hunchentoot))

(in-package :hdemo)

(defvar *h* (make-instance 'easy-acceptor :port 80))
(defvar *y* (make-instance 'easy-acceptor :port 80))


;; define a handler with the name my-greetings
(define-easy-handler (my-greatings :uri "/hello")
    (name)
  (setf (hunchentoot:content-type*) "text/html")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "hunchentoot test")
            (:link :rel "stylesheet" :type "text/css" :href "/css/reset.css")
            (:link :rel "stylesheet" :type "text/css" :href "/css/style.css")
            (:link :rel "stylesheet" :type "text/css" :href "http://fonts.googleapis.com/css?family=Ubuntu")
            (:link :rel "stylesheet" :type "text/css" :href "http://fonts.googleapis.com/css?family=Droid+Sans"))
     (:body
      (:h1 "hunchentoot form")
      (:div :class "container"
       (:form
        :method :post
        (:input :type :text
                :name "name"
                :value name)
        (:input :type :submit :value "submit your name"))
       (:p "Hello " (str name)))))))

(push (create-folder-dispatcher-and-handler
       "/css/" #p"~/bin/lisp/loving_cl/css/")
      *dispatch-table*)

;; (push (create-static-file-dispatcher-and-handler
;;        "/style.css" "stylesheets/style.css")
;;       *dispatch-table*)

(hunchentoot:start *h*)
(hunchentoot:start *y*)

;(hunchentoot:stop *h*)



;;(:script :src "/js/user.js" :type "text/javascript" "")
