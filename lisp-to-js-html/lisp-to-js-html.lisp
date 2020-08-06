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

(defmacro render-people-list->js&html-table (list-name)
  "parenscript macro to output table rows based on list"
  (let ((table-element (make-symbol (concatenate 'string list-name "-table")))
        (row-element (make-symbol (concatenate 'string list-name "-row")))
        (name-cell-element (make-symbol (concatenate 'string list-name "-name-cell")))
        (name-text-node (make-symbol (concatenate 'string list-name "-name-text-node")))
        (karma-cell-element (make-symbol (concatenate 'string list-name "-karma-cell")))
        (karma-text-node (make-symbol (concatenate 'string list-name "-karma-text-node"))))
    `(ps
       (defun render-people-list (people)
         (chain people (map
                        #'(lambda (person)
                            (let ((,table-element (chain document (get-element-by-id "sample-table")))
                                  (,row-element (chain document (create-element "tr")))
                                  (,name-cell-element (chain document (create-element "td")))
                                  (,name-text-node (chain document (create-text-node (@ person name))))
                                  (,karma-cell-element (chain document (create-element "td")))
                                  (,karma-text-node (chain document (create-text-node (@ person karma)))))
                              (chain ,name-cell-element (append-child ,name-text-node))
                              (chain ,karma-cell-element (append-child ,karma-text-node))
                              (chain ,row-element (append-child ,name-cell-element))
                              (chain ,row-element (append-child ,karma-cell-element))
                              (chain ,table-element (append-child ,row-element))))))))))

(defun render-people-list ()
  "wrapper around macro to generate Javascript"
  (ps (render-people-list->js&html-table "people")))

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
                                     (:tr
                                      (:td "Name")
                                      (:td "Karma")))))))))))

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
