(load "/usr/lib/quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)

(defstruct target x0 x1 y0 y1)

(defun load-target (filename)
  (with-open-file (file filename)
    (cl-ppcre:register-groups-bind
        (x0 x1 y0 y1)
        ("target area: x=(-?\\d*)\\.\\.(-?\\d*), y=(-?\\d*)\\.\\.(-?\\d*)" (read-line file))
      (make-target :x0 (parse-integer x0)
                   :x1 (parse-integer x1)
                   :y0 (parse-integer y0)
                   :y1 (parse-integer y1)))))

(defun test-y-velocity (target vel)
  (let ((pos 0) (max-pos nil))
    (loop (when (< pos (target-y0 target))
            (return))
          (when (<= pos (target-y1 target))
            (return max-pos))
          (setq pos (+ pos vel))
          (when (or (not max-pos) (> pos max-pos))
            (setq max-pos pos))
          (decf vel))))

(let* ((target (load-target "input"))
       (min-y-vel (target-y0 target))
       (max-y-vel (- min-y-vel))
       (y-vel min-y-vel)
       (max-y-pos nil))
  (loop while (<= y-vel max-y-vel)
        do (let ((p (test-y-velocity target y-vel)))
             (when (or (not max-y-pos) (and p (> p max-y-pos)))
               (setq max-y-pos p)))
           (incf y-vel))
  (format t "~a~%" max-y-pos))
