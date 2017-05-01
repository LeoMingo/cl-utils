(defpackage cl-utils
  (:export :make-adjustable-array
           :make-adjustable-string
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
           :incremental-arr-check
           :insert-str-all
    
           :fst
           :snd

           :read-str
           :read-line-arr
           :write-str
           :write-line-arr))


(defun make-adjustable-array (arr)
  (make-array (length arr)
              :fill-pointer (length arr)
              ;:fill-pointer t
              :adjustable t
              :initial-contents arr
              :element-type (array-element-type arr)))

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
            (empty-str (make-array '(0) 
                               :fill-pointer t
                               :adjustable t
                               :element-type 'character
                               :initial-contents ""))   )
  
        (dotimes (idx slen) 
            do      (if (equal (aref str idx) token) 
                        (progn 
                            (vector-push-extend str-temp str-arr)
                            (setf str-temp empty-str))
                        (setf str-temp (push-char (aref str idx) str-temp))))    

        (vector-push-extend str-temp str-arr)
        str-arr))




(defun concstr (&rest body)
  (let ((new-str ""))
    (dotimes (i (length body))
      (setf new-str (concatenate 'string new-str (nth i body))))
    (make-adjustable-string new-str)))

(defun concarr (&rest body)
  (let ((new-arr (make-array '(0) :adjustable t :fill-pointer t)))
    (dotimes (i (length body))
      (setf new-arr (concatenate 'array new-arr (nth i body))))
    (make-adjustable-array new-arr)))


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




(defun incremental-arr-check (base-arr checked-arr idx)
  (let ((bool-rst t)
        (ba-len (length base-arr))
        (checked-len (length checked-arr)))
    (if (> (+ checked-len idx) ba-len)  ;exclude the case of index of base-arr out range
      (setf bool-rst nil)
      (dotimes (i checked-len)
         (if (not (equal (aref checked-arr i) (aref base-arr (+ i idx))))
           (setf bool-rst nil))))
    bool-rst))


(defun insert-str-all (str-base str-tok str-ins)
  (let ((rst-str (make-adjustable-string ""))
        (strlen-1 (- (length str-tok) 1))
        (sblen (length str-base)))
    (dotimes (i (- sblen strlen-1))
      (if (incremental-arr-check str-base str-tok i)
        (progn (setf rst-str (concstr rst-str str-ins))
               (setf rst-str (push-char (aref str-base i) rst-str)))
        (setf rst-str (push-char (aref str-base i) rst-str))))
    (dotimes (i strlen-1)
      (setf rst-str (push-char (aref str-base (+ (- sblen strlen-1) i)) rst-str)))
   rst-str))




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

(defun flatten-array (arr)
"Flatten a 2-dimensional array into a 1-dimentional array"
  (let ((new-arr (make-adjustable-array #())))
    (dotimes (i (length arr))
      (setf new-arr (concarr new-arr (aref arr i))))
    new-arr))

(defun read-str (filename &key (newline nil))
  (let ((file-arr (read-line-arr filename))
        (temp-str "")
        (str ""))
    (if newline
      (progn
        (dotimes (i (1- (length file-arr)))
          (setf temp-str (push-char #\newline (aref file-arr i)))
          (setf str (concstr str temp-str)))
          ;;Lastly we set the last line without #\newline
          (setf str (concstr str (aref file-arr (1- (length file-arr))))))
      (setf str (flatten-array file-arr)))
    (char-arr->str str)))
  
  

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















