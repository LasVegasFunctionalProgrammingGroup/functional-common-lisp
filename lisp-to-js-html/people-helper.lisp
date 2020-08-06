(in-package :samples-webapp)

;; add dto for people here and some helper methods
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (karma
    :initarg :karma
    :initform 0
    :accessor karma)
   ))

(defun create-test-people ()
  (list
   (make-instance 'person :name "Bill" :karma 512)
   (make-instance 'person :name "George" :karma 256)
   (make-instance 'person :name "Barak" :karma 128)
   (make-instance 'person :name "Don" :karma 64)))

(defun get-person-by-name (search-name)
  (let ((some-test-people (create-test-people)))
    (find-if #'(lambda (a-person)
                 (string= search-name (name a-person))) some-test-people)))


