; all possible dice rolls
(defparameter *dice-rolls* (make-array 27))

; calculate dice rolls
(let ((i 0))
  (loop for a from 1 to 3
        do (loop for b from 1 to 3
                 do (loop for c from 1 to 3
                          do (setf (aref *dice-rolls* i) (+ a b c))
                             (incf i)))))

; cache of game states
(defparameter *game-state-cache*
  (make-array (list 10 21 10 21) :initial-element nil))

(defun game (p1-pos p1-score p2-pos p2-score)
  (or
    (aref *game-state-cache* p1-pos p1-score p2-pos p2-score)
    (let ((p1-wins 0) (p2-wins 0))
      (loop for roll across *dice-rolls*
            do (let* ((new-pos (mod (+ p1-pos roll) 10))
                      (new-score (+ p1-score new-pos 1)))
                 (cond
                   ((>= new-score 21)
                     (incf p1-wins))
                   (t
                     (let ((wins (game p2-pos p2-score new-pos new-score)))
                       (incf p1-wins (cdr wins))
                       (incf p2-wins (car wins)))))))
      (setf (aref *game-state-cache* p1-pos p1-score p2-pos p2-score)
        (cons p1-wins p2-wins)))))

(defun load-one-initial-pos (file)
  (let ((line (read-line file)))
    (- (parse-integer (subseq line (- (length line) 2))) 1)))

(defun load-initial-state (filename)
  (with-open-file (file filename)
    (values (load-one-initial-pos file)
            (load-one-initial-pos file))))

(multiple-value-bind (p1-pos p2-pos) (load-initial-state "input")
  (let ((wins (game p1-pos 0 p2-pos 0)))
    (format t "~a~%" (max (car wins) (cdr wins)))))
