(defun string-is-upper (str)
  (loop for c across str
        unless (upper-case-p c)
        do (return nil)
        finally (return t)))

(defun valid-path (path)
  (let ((small-caves nil)
        (many-count 0))
       (loop for cave in path
             unless (or (equal cave "start")
                        (equal cave "end")
                        (string-is-upper cave)
                        (find cave small-caves :test #'equal))
             do (setq small-caves (append small-caves `(,cave))))
       (loop for cave in small-caves
             as num = (count cave path :test #'equal)
             do (cond ((> num 2) (return nil))
                      ((= num 2) (setq many-count (+ many-count 1))
                                 (when (= many-count 2) (return nil))))
             finally (return t))))

(defun concat-paths (cave paths)
  (loop for path in paths
        as new-path = (append path `(,cave))
        when (valid-path new-path)
        collect new-path))

(defun visit-cave (cave paths neighbors)
  (if paths
      (let ((new-paths (concat-paths cave paths))
            (result nil))
           (if (equal cave "end")
               new-paths
               (progn (loop for neighbor in (gethash cave neighbors)
                            unless (equal neighbor "start")
                            do (let ((neighbor-paths (visit-cave neighbor new-paths neighbors)))
                                    (setq result (append result neighbor-paths))))
                      result)))))

(defun split-dashes (str)
  (let ((token "")
        (result nil))
       (loop for c across str
             when (eql c #\-)
             do (setq result (append result `(,token)))
                (setq token "")
             else
             do (setq token (concatenate 'string token `(,c))))
       (append result `(,token))))

(let ((neighbors (make-hash-table :test #'equal)))
     (with-open-file (file "input")
       (loop as line = (read-line file nil)
             while line
             do (let* ((parts (split-dashes line))
                       (u (first parts))
                       (v (second parts)))
                      (setf (gethash u neighbors)
                            (append (gethash u neighbors) `(,v)))
                      (setf (gethash v neighbors)
                            (append (gethash v neighbors) `(,u))))))
     (print (length (visit-cave "start" '(nil) neighbors))))
