;;; EDUARDO JANICAS 78974 | DIANA ANTUNES 82448 | AL113

(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st)
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir
(defun nextStates (st)
  "generate all possible next states"
 (let ((successors nil))
	(loop for action in (possible-actions) do
 (let ((new-state (nextState st action)))
		 (push new-state successors)))
	successors))

;;; Expand node
(defun expand (node problem)
  "Generate a list of all the nodes that can be reached from a node."
  ;; Note the problem's fn-nextstate returns a list of states.
  ;; This function turns each of these into a node.
  (let ((nodes nil))
	(loop for state in (reverse (funcall (problem-fn-nextStates problem) (node-state node))) do
		(let* ((g (+ (node-g node) 1))
	  	(h 0))
     	(push
      	(make-node :parent node :state state :g g :h h :f (+ g h))
      	nodes
			)
		)
	)
  nodes)
)

;;; Solution states
(defun solution-states (node &optional (nodes-so-far nil))
  "Return a list of the nodes along the path to the solution."
  (cond ((null node) nodes-so-far)
	(t (solution-states (node-parent node)
			   (cons (node-state node) nodes-so-far)))))

;;; DLS
(defun depth-limited-search (problem node limit &key)
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond ((funcall (problem-fn-isGoal problem) (node-state node)) node)
        ((>= (node-g node) limit) :cutoff)
        (t (loop for n in (expand node problem) do
						(let ((solution (depth-limited-search problem n limit)))
		  				(when (node-p solution) (return solution))
						)
					)
				)
	)
)

;;; limdepthfirstsearch
(defun limdepthfirstsearch (problem lim &key)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
	(let ((node (make-node :parent nil
												:state (problem-initial-state problem)
												:g 0 :h 0 :f 0
							)
				))
		(let ((solution (depth-limited-search problem node lim)))
			(cond ((eq solution :cutoff) :cutoff)
						((eq solution nil) nil)
						(t (solution-states solution))
			)
		)
	)
)

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
    st - initial state
    problem - problem information
    lim - limit of depth iterations"
  (loop for depth from 0 to lim do
    (let ((solution (limdepthfirstsearch problem depth)))
 	 			(when (and (not (eq solution nil)) (not (eq solution :cutoff))) (return solution))
		)
	)
)
