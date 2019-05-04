(defvar *triplets* 
      '((1 2 3) (4 5 6) (7 8 9) ; horizontal
	(1 4 7) (2 5 8) (3 6 9) ; vertical
	(1 5 9) (3 5 7)))       ; diagonal

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal 1 v) "O")
	((equal 10 v) "X")
	(T " ")))

(defun print-row (x y z)
  (format T "~&   ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format T "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format T "~&  -----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format T "~&  -----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format T "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar (lambda (triplet) (sum-triplet board triplet)) *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (cond ((y-or-n-p "Would you like to go first? ")
	 (setf *opponent* 10)
	 (setf *computer* 1)
	 (print-board (make-board))
	 (opponent-move (make-board)))
	(T (setf *computer* 10)
	   (setf *opponent* 1)
	   (computer-move (make-board)))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move 
		     *opponent*
		     pos
		     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format T "~&You win!"))
	  ((board-full-p new-board)
	   (format T "~&Tie game."))
	  (T (computer-move new-board)))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move 
		     *computer* pos board)))
    (format T "~&My move: ~S" pos)
    (format T "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format T "~&I win!"))
	  ((board-full-p new-board)
	   (format T "~&Tie game."))
	  (T (opponent-move new-board)))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun read-a-legal-move (board)
  (format T "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format T "~&Invalid input.")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format T "~&That space is already occupied.")
	   (read-a-legal-move board))
	  (T pos))))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))
