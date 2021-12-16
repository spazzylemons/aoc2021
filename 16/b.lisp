(defparameter *hex-digits* (make-hash-table))

(setf (gethash #\0 *hex-digits*) (list 0 0 0 0))
(setf (gethash #\1 *hex-digits*) (list 0 0 0 1))
(setf (gethash #\2 *hex-digits*) (list 0 0 1 0))
(setf (gethash #\3 *hex-digits*) (list 0 0 1 1))
(setf (gethash #\4 *hex-digits*) (list 0 1 0 0))
(setf (gethash #\5 *hex-digits*) (list 0 1 0 1))
(setf (gethash #\6 *hex-digits*) (list 0 1 1 0))
(setf (gethash #\7 *hex-digits*) (list 0 1 1 1))
(setf (gethash #\8 *hex-digits*) (list 1 0 0 0))
(setf (gethash #\9 *hex-digits*) (list 1 0 0 1))
(setf (gethash #\A *hex-digits*) (list 1 0 1 0))
(setf (gethash #\B *hex-digits*) (list 1 0 1 1))
(setf (gethash #\C *hex-digits*) (list 1 1 0 0))
(setf (gethash #\D *hex-digits*) (list 1 1 0 1))
(setf (gethash #\E *hex-digits*) (list 1 1 1 0))
(setf (gethash #\F *hex-digits*) (list 1 1 1 1))

(defstruct packet-reader
  bits
  len)

(defun read-input (filename)
  (with-open-file (file filename)
    (let* ((hex (read-line file))
           (len 0)
           (bits (apply #'append (map 'list (lambda (c)
                                               (incf len 4)
                                               (gethash c *hex-digits*))
                                             hex))))
      (make-packet-reader :bits bits :len len))))

(defun read-bit (reader)
  (decf (packet-reader-len reader))
  (pop (packet-reader-bits reader)))

(defun read-bits (n reader)
  (loop for nil below n collect (read-bit reader)))

(defun bits-to-int (bits)
  (let ((result 0))
    (loop for b in bits
          do (setq result (+ (* result 2) b)))
    result))

(defun read-int (n reader)
  (bits-to-int (read-bits n reader)))

(defstruct packet
  version
  id
  content)

(defun read-packet (reader)
  (let ((version (read-int 3 reader))
        (id (read-int 3 reader))
        (content 0)
        (chunk nil))
    (if (= id 4)
      (loop (setq chunk (read-bits 5 reader))
        (setq content (+ (* content 16) (bits-to-int (cdr chunk))))
        (when (= (car chunk) 0) (return)))
      (let ((length-type-id (read-bit reader)))
           (setf content (if (= length-type-id 0)
             (let* ((bits-to-read (read-int 15 reader))
                    (goal-len (- (packet-reader-len reader) bits-to-read)))
               (loop while (> (packet-reader-len reader) goal-len)
                     collect (read-packet reader)))
             (let ((subpackets-to-read (read-int 11 reader)))
               (loop for nil below subpackets-to-read
                     collect (read-packet reader)))))))
    (make-packet :version version
                 :id id
                 :content content)))

(defun packet-value (packet)
  (if (= (packet-id packet) 4)
    (packet-content packet)
    (let ((subvalues (map 'list #'packet-value (packet-content packet))))
      (case (packet-id packet)
        (0 (apply #'+ subvalues))
        (1 (apply #'* subvalues))
        (2 (apply #'min subvalues))
        (3 (apply #'max subvalues))
        (5 (if (> (car subvalues) (cadr subvalues)) 1 0))
        (6 (if (< (car subvalues) (cadr subvalues)) 1 0))
        (7 (if (= (car subvalues) (cadr subvalues)) 1 0))))))

(let ((reader (read-input "input")))
  (format t "~a~%" (packet-value (read-packet reader))))
