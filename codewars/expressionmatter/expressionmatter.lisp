(defun expression-matter (a b c)
  "determine which combination yields the max result"
  (labels ((rec-max (list)
	     (cond ((null list) 0)
		   (t (max (car list) (rec-max (cdr list)))))))
    (let ((funs (list
		 #'(lambda (x y z) (* x y z))
		 #'(lambda (x y z) (+ x y z))
		 #'(lambda (x y z) (* (+ x y) z))
		 #'(lambda (x y z) (* x (+ y z))))))
      (rec-max (mapcar #'(lambda (e) (funcall e a b c)) funs)))))

(defun expression-matter-simpler (a b c)
  (max
   (* a b c)
   (+ a b c)
   (* (+ a b) c)
   (* a (+ b c))))

;;; test section
(defvar *pass-total* 0)
(defvar *fail-total* 0)

(defun assert-equal (text expected actual)
  (format t "~&Testing ~s ... ~d ~a" text expected actual)
  (if (equal expected actual)
      (progn
	(format t "...passed!~%")
	(incf *pass-total*))
      (progn
	(format t "... FAILED!~%")
	(incf *fail-total*))))

(defun kata-tests ()
  ;; Basic tests
  (setf *pass-total* (setf *fail-total* 0))
  (assert-equal "expressionMatter 2 1 2" 6 (expression-matter-simpler 2 1 2))
  (assert-equal "expressionMatter 2 1 2" 6 (expression-matter-simpler 2 1 2))
  (assert-equal "expressionMatter 2 1 1" 4 (expression-matter-simpler 2 1 1))
  (assert-equal "expressionMatter 1 1 1" 3 (expression-matter-simpler 1 1 1))
  (assert-equal "expressionMatter 1 2 3" 9 (expression-matter-simpler 1 2 3))
  (assert-equal "expressionMatter 1 3 1" 5 (expression-matter-simpler 1 3 1))
  (assert-equal "expressionMatter 2 2 2" 8 (expression-matter-simpler 2 2 2))
  (assert-equal "expressionMatter 5 1 3" 20 (expression-matter-simpler 5 1 3))
  (assert-equal "expressionMatter 3 5 7" 105 (expression-matter-simpler 3 5 7))
  (assert-equal "expressionMatter 5 6 1" 35 (expression-matter-simpler 5 6 1))
  (assert-equal "expressionMatter 1 6 1" 8 (expression-matter-simpler 1 6 1))
  (assert-equal "expressionMatter 2 6 1" 14 (expression-matter-simpler 2 6 1))
  (assert-equal "expressionMatter 6 7 1" 48 (expression-matter-simpler 6 7 1))
  (assert-equal "expressionMatter 2 10 3" 60 (expression-matter-simpler 2 10 3))
  (assert-equal "expressionMatter 1 8 3" 27 (expression-matter-simpler 1 8 3))
  (assert-equal "expressionMatter 9 7 2" 126 (expression-matter-simpler 9 7 2))
  (assert-equal "expressionMatter 1 1 10" 20 (expression-matter-simpler 1 1 10))
  (assert-equal "expressionMatter 9 1 1" 18 (expression-matter-simpler 9 1 1))
  (assert-equal "expressionMatter 10 5 6" 300 (expression-matter-simpler 10 5 6))
  (assert-equal "expressionMatter 1 10 1" 12 (expression-matter-simpler 1 10 1))
  (format t "Total Tests: ~d~% Passed: ~d~% Failed: ~d~%" (+ *pass-total* *fail-total*) *pass-total* *fail-total*))

;; Random tests
(defun random-tests ()
  (labels ((sol (a b c)
	     (max 0
		  (* a (+ b c))
		  (* a b c)
		  (* (+ a b) c)
		  (+ a b c)
		  (* (+ a b) c)
		  (+ (* a b) c))))

    (setf *pass-total* (setf *fail-total* 0))

    (dotimes (_ 100)
      (setf a (random 10))
      (setf b (random 10))
      (setf c (random 10))
      (assert-equal (concat-string "expressionMatter " a b c) (sol a b c) (expression-matter-simpler a b c))))
  (format t "Total Tests: 100~%Passed: ~d~%Failed: ~d~%" *pass-total* *fail-total*))

(defun concat-string (text n1 n2 n3)
  (concatenate 'string text (write-to-string n1) (write-to-string n2) (write-to-string n3)))


