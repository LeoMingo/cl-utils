(defpackage cl-utils
  (:export :make-adjustable-string
           :push-char
           :split-at
           
           :trim-arr-edge
           :trim-nth
           :conc-str-arr 
           :concstr
           :concarr

           :char-arr->str
           
           :seq     ;A reverse of subseq

           :insert-arr
           :insert-str
           :fst
           :snd

           :read-str
           :read-line-arr
           :write-str
           :write-line-arr))




(defun make-adjustable-string (str) 
  (make-array (length str)
              :fill-pointer (length str)
              :adjustable t
              :initial-contents str
              :element-type (array-element-type str)))


(defun push-char (c str)
  (let ((ns (make-adjustable-string str)))
    (vector-push-extend c ns)
    ns))


;;Token character is ignored
(defun split-at (str token)
  (let  (   (slen (length str))
            (str-arr (make-array '(0)
                              :fill-pointer t
                              :adjustable t
                              :element-type 'string))
  
            (str-temp (make-array '(0) 
                               :fill-pointer t
                               :adjustable t
                               :element-type 'character
                               :initial-contents ""))
            (str-init (make-array '(0) 
                               :fill-pointer t
                               :adjustable t
                               :element-type 'character
                               :initial-contents ""))   )
  
        (dotimes (idx slen) 
            do      (if (equal (aref str idx) token) 
                        (progn 
                            (vector-push-extend str-temp str-arr)
                            (setf str-temp str-init))
                        (push-char (aref str idx) str-temp)))    

        (vector-push-extend str-temp str-arr)
        str-arr))




(defun concstr (&rest body)
  (let ((new-str ""))
    (dotimes (i (length body))
      (setf new-str (concatenate 'string new-str (nth i body))))
    new-str))

(defun concarr (&rest body)
  (let ((new-arr (make-array '(0) :adjustable t :fill-pointer t)))
    (dotimes (i (length body))
      (setf new-arr (concatenate 'array new-arr (nth i body))))
    new-arr))


(defun trim-arr-edge (arr edge)
  (let ((na (make-array (list (- (length arr) 1))
                       :adjustable t
                       :fill-pointer t
                       :element-type (array-element-type arr)))
        (len-1 (- (length arr) 1)))
    (dotimes (i len-1)
      (cond ((equal edge "s")  ;trim off the head of array
             (setf (aref na i) (aref arr (+ i 1))))
            ((equal edge "e")
             (setf (aref na i) (aref arr i)))
            (t (setf na arr)))
    )
    na))


(defun trim-nth (seq n)
  (let ((g (gensym)))
    (setf (elt seq n) g)
    (delete g seq)))

(defun conc-str-arr (str-arr)
  (let ((len (length str-arr))
        (str ""))
    (dotimes (i len)
      (setf str (concstr str (aref str-arr i))))
    str))

(defun char-arr->str (char-arr)
  (let ((len (length char-arr))
        (str (make-adjustable-string "")))
    (dotimes (i len)
      (setf str (push-char (aref char-arr i) str)))
    str))



;;a reverse version of subseq
(defun seq (arr n)
  (dotimes (i (- (length arr) n))
    (setf arr (trim-arr-edge arr "e")))
  arr)

(defun insert-arr (arr ele i)
  (let ((leftside (seq arr i))
        (rightside (subseq arr i)))
    (vector-push-extend ele leftside)
    (concarr leftside rightside)))



(defun insert-str (str-base str-inserted idx)
  (let ((csi (concatenate 'array str-inserted))
        (new-str (concatenate 'array str-base)))
       (dotimes (i (length csi))
         (setf new-str (insert-arr new-str (aref csi i)(+ i idx))))
    (char-arr->str new-str)))



(defun fst (arr) 
  (aref arr 0))
(defun snd (arr)
  (aref arr 1))




(defun read-line-arr (filename)
  (defparameter file-str-arr (make-array '(0) 
                                         :initial-element ""
                                         :element-type 'string
                                         :adjustable t
                                         :fill-pointer t))
  
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do
            (vector-push-extend line file-str-arr))
      (close in)))
  file-str-arr)


(defun read-str (filename)
  (let ((file-arr (read-line-arr filename))
        (temp-str "")
        (str ""))
    (dotimes (i (1- (length file-arr)))
      (setf temp-str (push-char #\newline (aref file-arr i)))
      (setf str (concstr str temp-str)))
    (setf str (concstr str (aref file-arr (1- (length file-arr)))))
    str))
  
  

(defun write-str (filename line)
  (with-open-file (stm filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (princ #\newline stm)
    (princ line stm)))

(defun write-line-arr (filename line-arr)
  (with-open-file (stm filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (dotimes (i (length line-arr))
      (princ #\newline stm)
      (princ (aref line-arr i)))))















