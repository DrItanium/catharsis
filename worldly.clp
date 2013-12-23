; Entry point into worldly 
; Modify this to fit your system.
(batch* (proton: /lib/core.clp))
(batch* (proton: /lib/neutron.clp))
; begin worldly 
(defclass pixel
  "Represents a location on a map"
  (is-a USER)
  (slot update
        (type SYMBOL)
        (storage local)
        (allowed-symbols FALSE TRUE))
  (slot position-x
        (type INTEGER)
        (storage local)
        (default ?NONE))
  (slot position-y
        (type INTEGER)
        (storage local)
        (default ?NONE))
  (slot type 
        (type SYMBOL)
        (allowed-symbols empty solid))
  (slot color
        (type SYMBOL)
        (default-dynamic black)))

(defglobal MAIN
           ?*block-width* = 32
           ?*block-height* = 32
           ?*screen-width* = (screen/dimensions/width)
           ?*screen-height* = (screen/dimensions/height)
           ?*blocks-wide* = (div ?*screen-width* ?*block-width*)
           ?*blocks-tall* = (div ?*screen-height* ?*block-height*)
           ?*block-count* = (* ?*blocks-wide* ?*blocks-tall*))

(defmethod on-resized
  "method to handle resizing of the window"
  ((?value SYMBOL (not (neq ?value FALSE TRUE))))
  (if (and ?value (< (getwindow) 0)) then
    (printout werror "ERROR: couldn't reattach to window" crlf)
    (exit))
  (bind ?*screen-width* (screen/dimensions/width))
  (bind ?*screen-height* (screen/dimensions/height))
  (bind ?*blocks-wide* (div ?*screen-width* ?*block-width*))
  (bind ?*blocks-tall* (div ?*screen-height* ?*block-height*))
  (bind ?*block-count* (* ?*blocks-wide* ?*blocks-tall*))
  (do-for-all-instances ((?pixel pixel)) 
                        (and (<= 0 (send ?pixel get-position-x)
                                   (- ?*blocks-wide* 1))
                             (<= 0 (send ?pixel get-position-y) ?*blocks-tall*))
                        (send ?pixel put-update TRUE))
  (modify-instance [screen-dim] (bx ?*screen-width*) (by ?*screen-height*))
  (send [screen-dim] build-pointer)
  (screen/draw [screen-dim] [screen] [ZP])
  (assert (layout pixels)))



(definstances screen-pieces 
              (scratch-rect of rectangle (x 0) (y 0) (bx 0) (by 0))
              (screen-dim of rectangle (x 0) (y 0) 
                          (bx ?*screen-width*) 
                          (by ?*screen-height*))
              (block of rectangle (x 0) (y 0) 
                     (bx ?*block-width*) 
                     (by ?*block-height*))
              (screen of image (rectangle [screen-dim])
                      (replicate TRUE)
                      (color (get-standard-color grey)))
              (solid-pixel of image 
                           (rectangle [block])
                           (replicate TRUE)
                           (color (get-standard-color black)))
              (empty-pixel of image 
                           (rectangle [block])
                           (replicate TRUE)
                           (color (get-standard-color blue))))
(deffacts query-operation
          (query input)
          (screen ?*blocks-wide* ?*blocks-tall*))


(defrule setup-pixels
         "Create the pixels we are going to use to represent the screen"
         (declare (salience 10000))
         ?f <- (screen ?w ?h)
         =>
         (retract ?f)
         (loop-for-count (?x 0 (- ?w 1)) do
                         (loop-for-count (?y 0 ?h) do
                                         (make-instance of pixel 
                                                        (position-x ?x)
                                                        (position-y ?y)
                                                        (type (if (= ?x ?y 0) then empty else solid))))))
(defrule query-input
         ?f <- (query input)
         =>
         (retract ?f)
         (send [mouse] query)
         (send [keyboard] query)
         (assert (check mouse)
                 (check keyboard)))

(defrule process-mouse-inputs
         (declare (salience -1))
         ?f <- (check mouse)
         =>
         (retract ?f)
         (assert (query mouse)))



(defrule process-keyboard-inputs:quit
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ESC))
         =>
         (retract ?f)
         (exit))

(defrule process-keyboard-inputs:left
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys LEFT))
         ?old <- (object (is-a pixel)
                         (type empty)
                         (position-x ?x)
                         (position-y ?y))
         (test (> ?x 0))
         ?next <- (object (is-a pixel)
                          (type solid)
                          (position-x ?bx&:(= ?bx (- ?x 1)))
                          (position-y ?y))

         =>
         (retract ?f)
         (modify-instance ?old (type solid)
                          (update TRUE))
         (modify-instance ?next (type empty)
                          (update TRUE))
         (assert (query keyboard)
                 (layout pixels)))

(defrule process-keyboard-inputs:right
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys RIGHT))
         ?old <- (object (is-a pixel)
                         (type empty)
                         (position-x ?x)
                         (position-y ?y))
         (test (< ?x ?*blocks-wide*))
         ?next <- (object (is-a pixel)
                          (type solid)
                          (position-x ?bx&:(= ?bx (+ ?x 1)))
                          (position-y ?y))

         =>
         (retract ?f)
         (modify-instance ?old (type solid)
                          (update TRUE))
         (modify-instance ?next (type empty)
                          (update TRUE))
         (assert (query keyboard)
                 (layout pixels)))

(defrule process-keyboard-inputs:up
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys UP))
         ?old <- (object (is-a pixel)
                         (type empty)
                         (position-x ?x)
                         (position-y ?y))
         (test (> ?y 0))
         ?next <- (object (is-a pixel)
                          (type solid)
                          (position-y ?by&:(= ?by (- ?y 1)))
                          (position-x ?x))

         =>
         (retract ?f)
         (modify-instance ?old (type solid) (update TRUE))
         (modify-instance ?next (type empty) (update TRUE))
         (assert (query keyboard)
                 (layout pixels)))

(defrule process-keyboard-inputs:down
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys DOWN))
         ?old <- (object (is-a pixel)
                         (type empty)
                         (position-x ?x)
                         (position-y ?y))
         (test (< ?y ?*blocks-tall*))
         ?next <- (object (is-a pixel)
                          (type solid)
                          (position-y ?by&:(= ?by (+ ?y 1)))
                          (position-x ?x))

         =>
         (retract ?f)
         (modify-instance ?old (type solid) (update TRUE))
         (modify-instance ?next (type empty) (update TRUE))
         (assert (query keyboard)
                 (layout pixels)))

(defrule process-keyboard-inputs:nil
         (declare (salience 1))
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys NIL))
         =>
         (retract ?f)
         (assert (query keyboard)))

(defrule process-keyboard-inputs
         ?f <- (check keyboard)
         (object (is-a keyboard)
                 (name [keyboard])
                 (keys ?b&~NIL))
         =>
         (retract ?f)
         (assert (query keyboard)))

(defrule ready-to-query-input-again 
         ?f <- (query keyboard)
         ?f2 <- (query mouse)
         =>
         (retract ?f ?f2)
         (send [mouse] clear)
         (send [keyboard] clear)
         (assert (query input)))

(defrule relayout:image
         (declare (salience 9999))
         (layout pixels)
         ?pixel <- (object (is-a pixel)
                           (update TRUE)
                           (type ?z)
                           (position-x ?x)
                           (position-y ?y))
         (object (is-a rectangle)
                 (name [block])
                 (bx ?bx)
                 (by ?by))
         =>
         (send ?pixel put-update FALSE)
         (modify-instance [scratch-rect] 
                          (x (* ?bx ?x))
                          (y (* ?by ?y))
                          (bx (* ?bx (+ ?x 1)))
                          (by (* ?by (+ ?y 1))))
         (send [scratch-rect] build-pointer)
         (screen/draw [scratch-rect] 
                      (symbol-to-instance-name (sym-cat ?z -pixel)) [ZP]))

(defrule relayout:image:done
         (declare (salience 9998))
         ?f <- (layout pixels)
         =>
         (retract ?f))

(batch* (proton: /lib/reset-run-exit.clp))
