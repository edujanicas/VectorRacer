;;; 78974 Eduardo Janicas 82448 Diana Antunes Grupo al113

;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun isObstaclep (pos track)
  "check if there is an obstacle at position pos of the track"
  (if (nth (second pos)
  		(nth (first pos)
			(track-env track)))
  nil t))


(defun isGoalp (st)
  "check if st is a goal state"
  (dolist (state (track-endpositions (state-track st)))
  	      (cond
		  		((equal state (state-pos st)) (return T))
				(T NIL)))
 )

(defun nextState (st act)
  "generate the nextState after state st and action act"
  	(defparameter midState (make-STATE
		:POS ( list (+ (car (state-pos st)) (car (state-vel st)) (car act))
			  (+ (cadr (state-pos st)) (cadr (state-vel st)) (cadr act)))
		:VEL ( list (+ (car (state-vel st)) (car act))
			  (+ (cadr (state-vel st)) (cadr act)))
		:ACTION act
		:TRACK (state-track st)
		:OTHER (state-other st)
	))
	(setf (state-cost midState) (cond
				((isObstaclep (state-pos midState) (state-track st)) 20)
				((isGoalp midState) -100)
				(T 1)))
	midState
)
