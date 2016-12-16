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

	;;;; The Queue datatype

	(defstruct q
	  (key #'identity)
	  (last nil)
	  (elements nil))

	(defun make-empty-queue () (make-q))

	(defun empty-queue? (q)
	  (= (length (q-elements q)) 0))

	(defun queue-front (q)
	  (elt (q-elements q) 0))

	(defun remove-front (q)
	  (if (listp (q-elements q))
	      (pop (q-elements q))
	    (heap-extract-min (q-elements q) (q-key q))))

	(defun enqueue-by-priority (q items key)
	  (setf (q-key q) key)
	  (when (null (q-elements q))
	    (setf (q-elements q) (make-heap)))
	  (loop for item in items do
	       (heap-insert (q-elements q) item key)))

	(defun make-initial-queue (problem queuing-fn)
		(let ((q (make-empty-queue)))
			(funcall queuing-fn q (list (create-start-node problem)))
			q))

	(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
	(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
	(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
	(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

	(defun heapify (heap i key)
	  (let ((l (heap-left i))
		(r (heap-right i))
		(N (- (length heap) 1))
		smallest)
	    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
						 (heap-val heap i key)))
			       l i))
	    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
		(setf smallest r))
	    (when (/= smallest i)
	      (rotatef (aref heap i) (aref heap smallest))
	      (heapify heap smallest key))))

	(defun heap-extract-min (heap key)
	  (let ((min (aref heap 0)))
	    (setf (aref heap 0) (aref heap (- (length heap) 1)))
	    (decf (fill-pointer heap))
	    (heapify heap 0 key)
	    min))

	(defun heap-insert (heap item key)
	  (vector-push-extend nil heap)
	  (let ((i (- (length heap) 1))
		(val (funcall key item)))
	    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
	      do (setf (aref heap i) (aref heap (heap-parent i))
		       i (heap-parent i)))
	    (setf (aref heap i) item)))

	(defun make-heap (&optional (size 100))
	  (make-array size :fill-pointer 0 :adjustable t))

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
    (dolist (act (possible-actions) successors)
      (let ((new-state (nextState st act)))
	(if (not (member new-state successors :test #'equalp))
	    (push new-state successors))))))

	;;; Solucao e uma seq ordenada de estados
	(defun solution (node)
	  (let ((seq-states nil))
	    (loop
	      (when (null node)
		(return))
	      (push (node-state node) seq-states)
	      (setf node (node-parent node)))
	    (values seq-states)))


	;;; limdepthfirstsearch
	(defun limdepthfirstsearch (problem lim &key cutoff?)
	  "limited depth first search
	     st - initial state
	     problem - problem information
	     lim - depth limit"
	  (labels ((limdepthfirstsearch-aux (node problem lim)
		     (if (isGoalp (node-state node))
			 (solution node)
			 (if (zerop lim)
			     :cutoff
			     (let ((cutoff? nil))
			       (dolist (new-state (nextStates (node-state node)))
				 (let* ((new-node (make-node :parent node :state new-state))
					(res (limdepthfirstsearch-aux new-node problem (1- lim))))
				   (if (eq res :cutoff)
				       (setf cutoff? :cutoff)
				       (if (not (null res))
					   (return-from limdepthfirstsearch-aux res)))))
			       (values cutoff?))))))
	    (let ((res (limdepthfirstsearch-aux (make-node :parent nil :state (problem-initial-state problem))
						problem
						lim)))
	      (if (eq res :cutoff)
		  (if cutoff?
		      :cutoff
		      nil)
		  res))))


	;iterlimdepthfirstsearch
	(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
	  "limited depth first search
	     st - initial state
	     problem - problem information
	     lim - limit of depth iterations"
	  (let ((i 0))
	    (loop
	      (let ((res (limdepthfirstsearch problem i :cutoff? T)))
		(when (and res (not (eq res :cutoff)))
		  (return res))
		(incf i)
		(if (> i lim)
		    (return nil))))))


;; Solution of phase 3

(defun expand (node problem)
(let ((nodes nil))
(loop for state in (reverse (funcall (problem-fn-nextStates problem) (node-state node))) do
(let* ((g (+ (node-g node)
	(state-cost state)))
  (h (funcall (problem-fn-h problem) state)))
   (push
    (make-node
     :parent node :state state
     :g g :h h
     :f (+ g h))
    nodes)))
  nodes))

(defun best-search (problem)
  (let ((nodes (make-initial-queue problem #'(lambda (old-q nodes) (enqueue-by-priority old-q nodes #'node-f))))
		node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (funcall (problem-fn-isGoal problem) (node-state node)) (RETURN (solution node)))
	  (funcall #'(lambda (old-q nodes) (enqueue-by-priority old-q nodes #'node-f)) nodes (expand node problem)))))

(defun create-start-node (problem)
	(let ((h (funcall (problem-fn-h problem) (problem-initial-state problem))))
		(make-node :state (problem-initial-state problem)
		:g 0 :h h :f h)))

;; Heuristic
(defun compute-heuristic (st)
	(let ((finish-pos most-positive-fixnum))
	(dolist (pos (track-endpositions (state-track st)))
	(if (< (pos-c pos) finish-pos)
	(setf finish-pos (pos-c pos))))
	(cond ((isGoalp st) 0)
				((isObstaclep (state-pos st) (state-track st)) (most-positive-fixnum))
				(T (- finish-pos (pos-c (state-pos st)))))
	)
)

;;; A*
(defun a* (problem)
  (best-search problem))