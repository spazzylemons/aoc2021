(load "/usr/lib/quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)

(defparameter *cuboid-pattern*
  (let* ((num "(-?\\d+)")
         (range (format nil "~a\\.\\.~a" num num)))
    (format nil "^(on|off) x=~a,y=~a,z=~a$" range range range)))

(defstruct cuboid x y z)

(defun parse-cuboid (line)
  (cl-ppcre:register-groups-bind
      (on x0 x1 y0 y1 z0 z1)
      (*cuboid-pattern* line)
    (values (string= on "on")
            (make-cuboid :x (cons (parse-integer x0) (parse-integer x1))
                         :y (cons (parse-integer y0) (parse-integer y1))
                         :z (cons (parse-integer z0) (parse-integer z1))))))

(defun no-axis-overlap (a b)
  (or (< (cdr a) (car b)) (> (car a) (cdr b))))

(defmacro when-ordered (l v)
  (let ((var (gensym)))
    `(let ((,var ,v))
       (when (<= (car ,var) (cdr ,var))
         (push ,var ,l)))))

(defun split-axis (original eraser)
  (if (no-axis-overlap original eraser)
    (list original)
    (let ((lhalf (< (car eraser) (car original)))
          (rhalf (> (cdr eraser) (cdr original))))
      (if (and lhalf rhalf)
        (list original)
        (let ((result nil))
          (cond
            (lhalf
              (when-ordered result (cons (car original) (cdr eraser))))
            (rhalf
              (when-ordered result (cons (car eraser) (cdr original))))
            (t
              (push eraser result)))
          (when-ordered result (cons (car original) (- (car eraser) 1)))
          (when-ordered result (cons (+ (cdr eraser) 1) (cdr original)))
          result)))))

(defun side-length (side)
  (+ (- (cdr side) (car side)) 1))

(defun area (cuboid)
  (* (side-length (cuboid-x cuboid))
     (side-length (cuboid-y cuboid))
     (side-length (cuboid-z cuboid))))

(defun intersects (c1 c2)
  (not (or (no-axis-overlap (cuboid-x c1) (cuboid-x c2))
           (no-axis-overlap (cuboid-y c1) (cuboid-y c2))
           (no-axis-overlap (cuboid-z c1) (cuboid-z c2)))))

(defun erase (original eraser)
  (let ((x-pieces (split-axis (cuboid-x original) (cuboid-x eraser)))
        (y-pieces (split-axis (cuboid-y original) (cuboid-y eraser)))
        (z-pieces (split-axis (cuboid-z original) (cuboid-z eraser)))
        (result nil))
    (dolist (x x-pieces)
      (dolist (y y-pieces)
        (dolist (z z-pieces)
          (let ((cuboid (make-cuboid :x x :y y :z z)))
            (unless (intersects cuboid eraser)
              (push cuboid result))))))
    result))

(defmacro push-all (l m)
  (let ((v (gensym)))
    `(dolist (,v ,l)
       (push ,v ,m))))

(with-open-file (file "input")
  (let ((cuboids nil))
    (loop as line = (read-line file nil)
          while line
          do (multiple-value-bind (on new-cuboid) (parse-cuboid line)
               (let ((new-cuboids nil))
                 (flet ((f (cuboid)
                          (when (intersects cuboid new-cuboid)
                            (push-all (erase cuboid new-cuboid) new-cuboids)
                            t)))
                   (setf cuboids (delete-if #'f cuboids)))
                 (push-all new-cuboids cuboids)
                 (when on (push new-cuboid cuboids)))))
    (format t "~a~%" (apply #'+ (map 'list #'area cuboids)))))
