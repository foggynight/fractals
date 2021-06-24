;;;
;; --- fractal-tree.lisp ---
;;
;; Construct a fractal tree by splitting each point into some number of branches
;; where a point is the end of a previous branch. Branches splay outwards from
;; the center starting at the tree points.
;;
;; The starting point is a special case where the branches split evenly
;; distributed away from the center with a horizonal split, onwards branches
;; split away from the angle of their parent branch, with each set of branches
;; in a split being spread across an angle which is equal to a configuration
;; variable.
;;
;; Points are defined by an x-y position represented as a list containing the x
;; and y components as well as a list of child points.
;;
;; Trees are defined as m-ary trees of points, where m is the number of children
;; of each inner point.
;;
;; Trees are drawn by drawing lines between points and their children, these
;; lines are referred to as branches.
;;
;; The screen is centered on (0, 0), points components are measured relative to
;; the center of the screen.
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :sdl2)

;;; CONFIG SECTION -------------------------------------------------------------

;; Screen width and height
(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)

;; Center points of the screen
(defparameter *center-x* (floor (/ *screen-width* 2)))
(defparameter *center-y* (floor (/ *screen-height* 2)))

;; Colors used to draw the background and branches
(defparameter *background-color* '(0 0 0 255))
(defparameter *branch-color* '(255 0 0 255))

;; Angle between a pair of split branches in radians
(defparameter *split-angle* (/ pi 4))
;; Number of branches to split each point into
(defparameter *split-count* 2)
;; Length of each branch -- Distance between start and end point
(defparameter *branch-length* 100)

;;; VEC2 SECTION ---------------------------------------------------------------

;; Perform vector addition on two x-y vectors, creating a new vector.
(defun vec2-add (v0 v1)
  `(,(+ (car v0) (car v1)) ,(+ (cadr v0) (cadr v1))))

;; Perform vector subtraction on two x-y vectors, creating a new vector.
(defun vec2-sub (v0 v1)
  `(,(- (car v0) (car v1)) ,(- (cadr v0) (cadr v1))))

;; Perform scalar multiplication on an x-y vector, creating a new vector.
(defun vec2-mul (v c)
  `(,(* (car v) c) ,(* (cadr v) c)))

;; Perform scalar division on an x-y vector, creating a new vector.
(defun vec2-div (v c)
  `(,(/ (car v) c) ,(/ (cadr v) c)))

;; Check if two x-y vectors are equal.
(defun vec2-eq (v0 v1)
  (and (= (car v0)
          (car v1))
       (= (cadr v0)
          (cadr v1))))

;; Determine the length of an x-y vector.
(defun vec2-length (v)
  (sqrt (+ (expt (car v) 2) (expt (cadr v) 2))))

;; Convert an x-y vector into an angle in radians relative to the positive
;; x-axis with a positive rotation.
(defun vec2-to-angle (v)
  (let ((x (car v))
        (y (cadr v))
        (angle 0))
    (if (= x 0)
        (cond ((> y 0) (setq angle 90))
              ((< y 0) (setq angle 270))
              (t (setq angle 0)))
        (progn (setq angle (atan (/ y x)))
               ;; Adjust angle for correct quadrant
               (cond ((< x 0) (setq angle (+ angle 180)))
                     ((< y 0) (setq angle (+ angle 360))))))
    angle))

;;; TREE SECTION ---------------------------------------------------------------

(defun split-point (last-point start-point)
  )

(defun split-first-point (first-point)
  )

;;; RENDER SECTION -------------------------------------------------------------

;; Set the draw color of the renderer.
(defun render-set-color (ren color)
  (apply #'sdl2:set-render-draw-color (cons ren color)))

;; Fill the screen with the background color.
(defun render-clear (ren)
  (render-set-color ren *background-color*)
  (sdl2:render-clear ren))

;; Draw a branch in the branch color, starting and ending at the given points.
(defun render-draw-branch (ren start-point end-point)
  (render-set-color ren *branch-color*)
  (sdl2:render-draw-line ren
                         (floor (+ (car start-point) *center-x*))
                         (floor (+ (cadr start-point) *center-y*))
                         (floor (+ (car end-point) *center-x*))
                         (floor (+ (cadr end-point) *center-y*))))

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Fractal Tree"
                           :w *screen-width*
                           :h *screen-height*
                           :flags '(:resizable :shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym key)
           (when (sdl2:scancode= (sdl2:scancode-value key) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (render-clear ren)
           (sdl2:render-present ren))
          (:quit () t))))))
