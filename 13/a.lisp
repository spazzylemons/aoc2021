(defun substring-after (prefix str)
  (when (string= prefix (subseq str 0 (min (length prefix) (length str))))
    (subseq str (length prefix))))

(defun get-coordinates (str)
  (let ((token "")
        (result nil))
    (loop for c across str
          when (eql c #\,)
          do (setq result (append result (list (parse-integer token))))
             (setq token "")
          else
          do (setq token (concatenate 'string token (list c))))
    (unless (string= token "")
      (setq result (append result (list (parse-integer token)))))))

(defun foldxp (midpoint)
  (lambda (n) (< (first n) midpoint)))

(defun foldyp (midpoint)
  (lambda (n) (< (second n) midpoint)))

(defun fold-points-x (points midpoint)
  (loop for point in points
        collect (multiple-value-bind (x y) (values-list point)
                  (list (- midpoint (- x midpoint)) y))))

(defun fold-points-y (points midpoint)
  (loop for point in points
        collect (multiple-value-bind (x y) (values-list point)
                  (list x (- midpoint (- y midpoint))))))

(let ((points nil))
  (with-open-file (file "input")
    (loop as line = (read-line file nil)
          while line
          do (let ((data nil))
               (cond
                 ((setq data (substring-after "fold along x=" line))
                  (let* ((midpoint (parse-integer data))
                         (folded-values (remove-if (foldxp midpoint) points))
                         (mirrored-values (fold-points-x folded-values midpoint)))
                    (setq points (union mirrored-values (set-difference points folded-values) :test #'equal))
                    (print (length points))
                    (return)))
                 ((setq data (substring-after "fold along y=" line))
                  (let* ((midpoint (parse-integer data))
                         (folded-values (remove-if (foldyp midpoint) points))
                         (mirrored-values (fold-points-y folded-values midpoint)))
                    (setq points (union mirrored-values (set-difference points folded-values) :test #'equal))
                    (print (length points))
                    (return)))
                 ((setq data (get-coordinates line))
                  (setq points (union points (list data)))))))))
