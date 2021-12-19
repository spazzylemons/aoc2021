(defun read-cave-lines (filename)
  (with-open-file (file filename)
    (let ((lines nil))
      (loop as line = (read-line file nil)
            while line
            do (push (coerce line 'list) lines))
      lines)))

(defun risk-inc (risk)
  (if (= risk 9) 1 (+ risk 1)))

(defun load-cave (filename)
  (let* ((lines (read-cave-lines filename))
         (width (length lines))
         (height (length (car lines)))
         (cave (make-array (list (* 5 width) (* 5 height))))
         (i (- height 1)))
    (loop while lines
          do (let ((line (car lines))
                   (j 0))
               (loop while line
                     do (let ((r (parse-integer (coerce (list (car line)) 'string))))
                          (setf (aref cave (+ i (* width 0)) (+ j (* height 0))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 1)) (+ j (* height 0))) r)
                          (setf (aref cave (+ i (* width 0)) (+ j (* height 1))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 2)) (+ j (* height 0))) r)
                          (setf (aref cave (+ i (* width 1)) (+ j (* height 1))) r)
                          (setf (aref cave (+ i (* width 0)) (+ j (* height 2))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 3)) (+ j (* height 0))) r)
                          (setf (aref cave (+ i (* width 2)) (+ j (* height 1))) r)
                          (setf (aref cave (+ i (* width 1)) (+ j (* height 2))) r)
                          (setf (aref cave (+ i (* width 0)) (+ j (* height 3))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 4)) (+ j (* height 0))) r)
                          (setf (aref cave (+ i (* width 3)) (+ j (* height 1))) r)
                          (setf (aref cave (+ i (* width 2)) (+ j (* height 2))) r)
                          (setf (aref cave (+ i (* width 1)) (+ j (* height 3))) r)
                          (setf (aref cave (+ i (* width 0)) (+ j (* height 4))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 4)) (+ j (* height 1))) r)
                          (setf (aref cave (+ i (* width 3)) (+ j (* height 2))) r)
                          (setf (aref cave (+ i (* width 2)) (+ j (* height 3))) r)
                          (setf (aref cave (+ i (* width 1)) (+ j (* height 4))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 4)) (+ j (* height 2))) r)
                          (setf (aref cave (+ i (* width 3)) (+ j (* height 3))) r)
                          (setf (aref cave (+ i (* width 2)) (+ j (* height 4))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 4)) (+ j (* height 3))) r)
                          (setf (aref cave (+ i (* width 3)) (+ j (* height 4))) r)
                          (setq r (risk-inc r))
                          (setf (aref cave (+ i (* width 4)) (+ j (* height 4))) r))
                        (incf j)
                        (setq line (cdr line)))
               (decf i)
               (setq lines (cdr lines))))
    cave))

(defun try-neighbor (cave unvisited tentative current i j)
  (when (and (>= i 0) (< i (array-dimension cave 0))
             (>= j 0) (< j (array-dimension cave 1))
             (gethash `(,i . ,j) unvisited))
    (let* ((tentative-distance (aref tentative i j))
           (calculated-distance (+ current (aref cave i j))))
      (when (or (not tentative-distance)
                (< calculated-distance tentative-distance))
        (setf (aref tentative i j) calculated-distance)))))

(defun find-smallest-tentative (unvisited tentative)
  (let ((s nil) (sp nil))
    (maphash (lambda (k v)
      (let ((f (aref tentative (car k) (cdr k))))
        (when (and f (or (not s) (< f s)))
          (setq s f)
          (setq sp k))))
      unvisited)
    sp))

(let* ((cave (load-cave "input"))
       (unvisited (make-hash-table :test #'equal))
       (tentative (make-array (array-dimensions cave) :initial-element nil))
       (i 0) (j 0))
  (setf (aref tentative 0 0) 0)
  (loop for x below (array-dimension cave 0)
        do (loop for y below (array-dimension cave 1)
                 do (setf (gethash `(,x . ,y) unvisited) t)))
  (loop while (gethash `(,(- (array-dimension cave 0) 1) . ,(- (array-dimension cave 1) 1)) unvisited)
        do (let ((current (aref tentative i j)))
             (try-neighbor cave unvisited tentative current (- i 1) j)
             (try-neighbor cave unvisited tentative current (+ i 1) j)
             (try-neighbor cave unvisited tentative current i (- j 1))
             (try-neighbor cave unvisited tentative current i (+ j 1)))
    (remhash `(,i . ,j) unvisited)
    (let ((spos (find-smallest-tentative unvisited tentative)))
      (setq i (car spos))
      (setq j (cdr spos))))
  (print (aref tentative (- (array-dimension cave 0) 1) (- (array-dimension cave 1) 1))))