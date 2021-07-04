;;; --- fractal-tree.lisp ---
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

(require :sdl2)

;;; CONFIG SECTION -------------------------------------------------------------

(defparameter *screen-width* 1280
  "Screen width in pixels.")
(defparameter *screen-height* 720
  "Screen height in pixels.")

(defparameter *target-fps* 144
  "Target frames to render per second.")
(defparameter *frame-delay-ms* (round (/ 1000 *target-fps*))
  "Time to sleep between frames in milliseconds.")
(defparameter *update-delay-ms* 1000
  "Time between updates in milliseconds.")
(defparameter *update-delay-frame-count* (round (/ *update-delay-ms* *frame-delay-ms*))
  "Number of frames between updates.")

(defparameter *center-x* (floor (/ *screen-width* 2))
  "x coordinate at the center of the screen.")
(defparameter *center-y* (floor (/ *screen-height* 2))
  "y coordinate at the center of the screen.")

(defparameter *background-color* '(0 0 0 255)
  "Color used to draw the background.")
(defparameter *branch-color* '(255 0 0 255)
  "Color used to draw the branches.")

(defparameter *split-angle* (/ pi 3)
  "Angle between a pair of split branches in radians.")
(defparameter *split-count* 2
  "Number of branches to split each point into.")
(defparameter *branch-length* 100
  "Length of each branch -- Distance between start and end point.")
(defparameter *branch-shrink-factor* 0.8
  "Factor by which to shrink the branches per iteration.")

;;; TREE-POINT SECTION ---------------------------------------------------------

(defclass tree-point ()
  ((x
    :accessor x
    :initarg :x
    :documentation "x position of this tree-point.")
   (y
    :accessor y
    :initarg :y
    :documentation "y position of this tree-point.")
   (ancestor-count
    :accessor ancestor-count
    :initarg :ancestor-count
    :documentation "Number of ancestors of this tree-point.")
   (parent-angle
    :accessor parent-angle
    :initarg :parent-angle
    :documentation "Angle of the parent branch of this tree-point."))
  (:documentation
   "Tree-point class representing a point in the tree, each point has a parent
point and some number of child points; branches are lines drawn between a parent
and child point."))

(defun split-root-point (start-point)
  "Split the root point into its child points."
  (let ((point-list nil)
        (point nil)
        (angle 0)
        (delta-angle (/ (* 2 pi) *split-count*)))
    (dotimes (i *split-count*)
      (setq point (make-instance 'tree-point
                                 :x (+ (x start-point)
                                       (* *branch-length*
                                          (cos angle)))
                                 :y (+ (y start-point)
                                       (* *branch-length*
                                          (sin angle)))
                                 :ancestor-count 1
                                 :parent-angle angle))
      (setq angle (+ angle delta-angle))
      (setq point-list (cons point point-list)))
    point-list))

(defun split-point (start-point)
  "Split a child point into its child points."
  (let ((point-list nil)
        (point nil)
        (angle (- (parent-angle start-point)
                  (/ *split-angle* 2)))
        (delta-angle (/ *split-angle* (1- *split-count*))))
    (dotimes (i *split-count*)
      (setq point (make-instance 'tree-point
                                 :x (+ (x start-point)
                                       (* *branch-length*
                                          (expt *branch-shrink-factor* (ancestor-count start-point))
                                          (cos angle)))
                                 :y (+ (y start-point)
                                       (* *branch-length*
                                          (expt *branch-shrink-factor* (ancestor-count start-point))
                                          (sin angle)))
                                 :ancestor-count (1+ (ancestor-count start-point))
                                 :parent-angle angle))
      (setq angle (+ angle delta-angle))
      (setq point-list (cons point point-list)))
    point-list))

;;; RENDER SECTION -------------------------------------------------------------

(defun render-set-color (ren color)
  "Set the draw color of the renderer."
  (apply #'sdl2:set-render-draw-color (cons ren color)))

(defun render-draw-background (ren)
  "Fill the screen with the background color."
  (render-set-color ren *background-color*)
  (sdl2:render-clear ren))

(defun render-draw-branch (ren start-point end-point)
  "Draw a branch in the branch color using a line thats starts and ends at the
given points."
  (render-set-color ren *branch-color*)
  (sdl2:render-draw-line ren
                         (floor (+ (x start-point) *center-x*))
                         (floor (+ (y start-point) *center-y*))
                         (floor (+ (x end-point) *center-x*))
                         (floor (+ (y end-point) *center-y*))))

(defun render-draw-branch-list (ren start-point end-point-list)
  "Draw a list of branches using a starting point and list of end points."
  (dolist (end-point end-point-list)
    (render-draw-branch ren start-point end-point)))

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (let* ((start-point (make-instance 'tree-point
                                     :x 0 :y 0
                                     :ancestor-count 0
                                     :parent-angle 0))
         (point-list (split-root-point start-point))
         (next-point-list nil)
         (frame-count 0))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "Fractal Tree"
                             :w *screen-width*
                             :h *screen-height*
                             :flags '(:shown))
        (sdl2:with-renderer (ren win :flags '(:accelerated))
          (render-draw-background ren)
          (render-draw-branch-list ren start-point point-list)
          (sdl2:render-present ren)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym key)
             (when (sdl2:scancode= (sdl2:scancode-value key) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (unless (< frame-count *update-delay-frame-count*)
               (setq frame-count 0)
               (dolist (point point-list)
                 (let ((split-point-list (split-point point)))
                   (render-draw-branch-list ren point split-point-list)
                   (setq next-point-list (concatenate 'list next-point-list split-point-list))))
               (sdl2:render-present ren)
               (setq point-list next-point-list)
               (setq next-point-list nil))
             (incf frame-count)
             (sdl2:delay *frame-delay-ms*))
            (:quit () t)))))))
