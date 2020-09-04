
(ql:quickload '(cl-who hunchentoot parenscript cl-json))

(defpackage :samples-webapp
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :samples-webapp)

(defun start-server (port)
  "start web server - include re-start in case server is already running"
  (restart-case (start (make-instance 'easy-acceptor :port port))
    (re-start-server ()
      :report "Restart Web Server"
      (stop-server *the-http-server*)
      (start-server port))))

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun publish-static-content ()
  "server static assets - css, js, html, images, etc"
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

(defvar *people-list*)

(defun start-web-app ()
  "start app-level concerns"
  (setf *people-list* (create-test-people))
        
  (publish-static-content))

(defun get-people-list ()
  "make REST call to get list of names"
  (ps
    (defun get-people-list ()
      (flet ((req-listener ()
               (let ((names (chain -j-s-o-n (parse (@ this response-text)))))
                 (render-people-list names))))

        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/person"))
          (chain o-req (send)))))))

(defun cons-pair-p (possible-cons)
  (or
   (and (consp possible-cons) (atom (cdr possible-cons)))
   (and (consp possible-cons) (listp (cdr possible-cons)) (equal '~f (cadr possible-cons)))))

(defun define-ps-with-html-macro ()
  "parenscript macro to output table rows based on list"
  (ps
    (defun create-an-element (parent-element tag)
      (let ((new-element (chain document (create-element tag))))
        (chain parent-element (append-child new-element))
        new-element))
    (defun set-an-attribute (parent-element key value)
      (chain parent-element (set-attribute key value)))
    (defun set-text-node (parent-element text)
      (let ((a-text-node (chain document (create-text-node text))))
        (chain parent-element (append-child a-text-node))))
    (defmacro with-html-elements (elements)
      (labels
          ((process-tag-r (element &optional (parent nil parent-supplied-p))
             (let* ((tag (car element))
                    (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                    (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element"))))
               (cons
                `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
                (mapcar
                 #'(lambda (e)
                     (cond
                       ((cons-pair-p e)
                        `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e))))
                       ((stringp e)
                        `(set-text-node ,parent-element ,e))
                       ((listp e)
                        `(progn
                           ,@(process-tag-r e parent-element)))
                       ((symbolp e)
                        `(set-text-node ,parent-element ,e))))
                 (cdr element))))))
        `(progn ,@(process-tag-r elements))))))
  
(defun render-people-list ()
  (ps
    (defun render-people-list (people)
      (let* ((todo-list-table-body (chain document (get-element-by-id "sample-table-body")))
             (parent-element todo-list-table-body))
        (chain people (map
                       #'(lambda (person)
                           (let ((name (@ person name))
                                 (karma (@ person karma)))
                             (with-html-elements
                                 (tr
                                  (td name) (td karma)))))))))))

(defun in-line-javascript ()
  "Javascript that doesn't live inside a function"
  (ps (defun people-list-handler (evt)
        (chain evt (prevent-default))
        (get-people-list))
      (defun init ()
        (setf people-list-button (chain document
                                      (get-element-by-id "get-people-btn")))
        (chain people-list-button
               (add-event-listener "click" people-list-handler false)))
      (setf (chain window onload) init)))

(defun make-people-page ()
  "web page with people info"
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Sample Input Form")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css")
            (:script :type "text/javascript"
                     (str (stringify
                           (define-ps-with-html-macro)
                           (render-people-list)
                           (get-people-list)
                           (in-line-javascript)))))
           (:body
            (:div
             (:h1 "Person Info"
                  (:div :id "sample-div"
                        (:h4 "Click here to get a list of names"
                             (:button :id "get-people-btn" "Get People List")
                             (:table :id "sample-table" :border "1" :cellspacing "0" :cellpadding "5"
                                     (:thead
                                      (:th "Name")
                                      (:th "Karma")
                                      (:tbody :id "sample-table-body")))))))))))

(define-easy-handler (people-page :uri "/people") ()
  "url handler for people info page"
  (make-people-page))

(defun get-person (name)
  "wrapper to get person info by name and serialize as JSON"
  (let ((person (get-person-by-name name)))
    (json:encode-json-to-string person)))

(defun get-everyone ()
  "wrapper to get info for all people and serialize as JSON"
  (json:encode-json-to-string (create-test-people)))
  
(define-easy-handler (person-info :uri "/person") (name)
  "REST endpoint for person info"
  (setf (content-type*) "application/json")
  (if name
      (get-person name)
      (get-everyone)))

(defun stop-server (server)
  "stop web server"
  (stop server))

(defparameter *the-http-server* (start-server 5050))

(start-web-app)
