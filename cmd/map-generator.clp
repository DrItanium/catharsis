; transform a map input file into a editable rule set
(batch* (proton: /lib/core.clp))

(defrule on-startup
         ?f <- (name ?name)
         =>
         (retract ?f)
         (format t "(definstances %s-entities %n" ?name)
         (assert (build-instances)))

(defrule generate-player-template
         (build-instances)
         ?f <- (player ?x ?y)
         =>
         (retract ?f)
         (format t "([player] of actor (position-x %d) (position-y %d))%n" 
                 ?x ?y))

(defrule generate-pixels-base
         (build-instances)
         ?f <- (build map ?w ?h { ?ex ?ey $?rest })
         =>
         (retract ?f)
         (assert (pixel empty ?ex ?ey)
                 (build map ?w ?h { $?rest })))

(defrule generate-pixels-base:finished
         (build-instances)
         ?f <- (build map ?w ?h { })
         =>
         (retract ?f)
         (loop-for-count (?x 0 ?w) do
                         (loop-for-count (?y 0 ?h) do
                                         (assert (pixel solid ?x ?y)))))

(defrule eliminate-collisions
         (build-instances)
         ?f <- (pixel solid ?x ?y)
         (pixel empty ?x ?y)
         =>
         (retract ?f))

(defrule generate-pixel 
         (declare (salience -1))
         (build-instances)
         ?f <- (pixel ?type ?x ?y)
         =>
         (retract ?f)
         (format t "( of pixel (type %s) (position-x %d) (position-y %d))%n" 
                 ?type ?x ?y))


(defrule close-build
         (declare (salience -2))
         ?f <- (build-instances)
         =>
         (retract ?f)
         (printout t ")" crlf))
