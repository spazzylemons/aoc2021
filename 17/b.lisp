(load "/usr/lib/quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)

(defstruct target x0 x1 y0 y1)

(defstruct probe
  (x-pos 0)
  (y-pos 0)
  (x-vel)
  (y-vel))

(defun approach-zero (value)
  (cond
    ((> value 0) (- value 1))
    ((< value 0) (+ value 1))
    (t 0)))

(defun probe-move (probe)
  (setf (probe-x-pos probe) (+ (probe-x-pos probe) (probe-x-vel probe)))
  (setf (probe-y-pos probe) (+ (probe-y-pos probe) (probe-y-vel probe)))
  (setf (probe-x-vel probe) (approach-zero (probe-x-vel probe)))
  (decf (probe-y-vel probe)))

(defun load-target (filename)
  (with-open-file (file filename)
    (cl-ppcre:register-groups-bind
        (x0 x1 y0 y1)
        ("target area: x=(-?\\d*)\\.\\.(-?\\d*), y=(-?\\d*)\\.\\.(-?\\d*)" (read-line file))
      (make-target :x0 (parse-integer x0)
                   :x1 (parse-integer x1)
                   :y0 (parse-integer y0)
                   :y1 (parse-integer y1)))))

(defun test-velocity (target x-vel y-vel)
  (let ((probe (make-probe :x-vel x-vel :y-vel y-vel)))
    (loop (when (< (probe-y-pos probe) (target-y0 target))
            (return))
          (when (and (>= (probe-y-pos probe) (target-y0 target))
                     (<= (probe-y-pos probe) (target-y1 target))
                     (>= (probe-x-pos probe) (target-x0 target))
                     (<= (probe-x-pos probe) (target-x1 target)))
            (return t))
          (probe-move probe))))

(let* ((target (load-target "input"))
       (min-y-vel (target-y0 target))
       (max-y-vel (- min-y-vel))
       (min-x-vel 0)
       (max-x-vel (target-x1 target))
       (x-vel nil)
       (y-vel min-y-vel)
       (trajectories 0))
  (loop while (<= y-vel max-y-vel)
        do (setq x-vel min-x-vel)
           (loop while (<= x-vel max-x-vel)
                 do (when (test-velocity target x-vel y-vel)
                      (incf trajectories))
                    (incf x-vel))
           (incf y-vel))
  (format t "~a~%" trajectories))