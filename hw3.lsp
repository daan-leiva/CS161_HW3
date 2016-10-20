;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
	(= v blank)
)

(defun isWall (v)
	(= v wall)
)

(defun isBox (v)
	(= v box)
)

(defun isKeeper (v)
	(= v keeper)
)

(defun isStar (v)
	(= v star)
)

(defun isBoxStar (v)
	(= v boxstar)
)

(defun isKeeperStar (v)
	(= v keeperstar)
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; This function iterates through each row of the state s and calls goal-test-row
; and returns through if all row calls return trye as well.
;
; s - state of the game
(defun goal-test (s)
	(cond
		((null s)					nil)
		; test if nil is empty
		((null (rest s))			(goal-test-row (first s)))
		(t 							(and (goal-test-row (first s)) (goal-test (rest s))))
		; check if the first item is a row
	)
);end defun

  
; This function checks if any items in the sRow list is of box type. If so it returns nil
; if the end is found then it returns t

; sRow - single row of a game state s
(defun goal-test-row (sRow)
	(cond
		((null sRow)											nil)
		; check if null
		((or (isBox (first sRow)))								nil)
		; c
		((null (rest sRow))										t)
		(t														(goal-test-row (rest sRow)))
		; if the item isn't a box then return an iterative call on the rest of the row
	)
)
  

;
; This function returns a list of all the possible state when moving the keeper in one
; of the four Cardinal directions: UP, DOWN, LEFT, Right
;
; Our state s is defined as the grid where the top left most corner is (0, 0). Moving to the rihgt
; increase the column count (0, 1) moving down increases the row count (1, 0)

; UP : (-1 0)
; DOWN : (1 0)
; LEFT : (0 1)
; RIGHT : (0 -1)
;
; Try move will return the result of attempting to move in that direction (nil or new a state).
; This results are combined into a single list and cleanUpList removes and nill items and returns
; a final list of possible moves
; 
; s - current state of our game
(defun next-states (s)
	(let*
		(
			(result (list (try-move s '(1 0)) (try-move s '(-1 0)) (try-move s '(0 -1)) (try-move s '(0 1))))
		)
		(cleanUpList result);end
	);end let
);

; This function returns the value in state s at a specified row (r), column (c)
; It iteratively tries to find the correct row. Once it does it passes the row to
; get-square-from-row to find the correct column value and returns this value
;
; s - current state of the game
; r - row at which we are searching for a value
; c - column at which we are searching for a value
(defun get-square (s r c)
	(cond
		((or (null s) (null r) (null c))		nil)
		((= r 0)								(get-square-from-row (first s) c))
		(t										(get-square (rest s) (- r 1) c))
	)
)

; This function returns the value at position c in the list sRow.
;
; sRow - single row from game 
(defun get-square-from-row (sRow c)
	(cond
		((null sRow)	nil)
		((= c 0)		(first sRow))
		(t				(get-square-from-row (rest sRow) (- c 1)))
	)
)

(defun set-square (s r c v)
	(cond
		((or (null s) (null r) (null c) (null v))		nil)
		((> r 0)										(append (list (first s)) (set-square (rest s) (- r 1) c v)))
		(t												(append (list (set-square-for-row (first s) c v)) (rest s)))
	)
)

(defun set-square-for-row (sRow c v)
	(cond
		((or (null sRow) (null c) (null v))			nil)
		; check null
		((= c 0)									(cons v (rest sRow)))
		(t											(cons (first sRow) (set-square-for-row (rest sRow) (- c 1) v)))
	)
)


; D - List of Direction Movement
	;(0, 1) 	- Right
	;(1, 0) 	- Down
	;(0, -1)	- Left
	;(-1, 0)	- Top
(defun try-move (s D)
	(cond
		((or (null s) (null D))			nil)
		(t
			(let*
				(
					(keeperRow 					(second (getKeeperPosition s 0)))
					(keeperCol 					(first (getKeeperPosition s 0)))
					(newKeeperRow				(+ keeperRow (first D)))
					(newKeeperCol				(+ keeperCol (second D)))
					(newBoxRow					(+ newKeeperRow (first D))) ; if no box this values
					(newBoxCol					(+ newKeeperCol (second D)))
					(keeperType					(get-square s keeperRow keeperCol))
					(contentOfNewPosition		(get-square s newKeeperRow newKeeperCol))
					(contentOfNewBoxPosition	(get-square s newBoxRow newBoxCol))
				)
				(cond
					((null (is-move-valid keeperType contentOfNewPosition contentOfNewBoxPosition))	nil)
					; check if move is valid
					(t
						(let*
							(
								(newKeeperContent 			(if (isKeeper keeperType) blank star))
								(newEmptyContent 			(if (or (isBlank contentOfNewPosition) (isBox contentOfNewPosition)) keeper keeperstar))
								
								(beforeBoxMoveState			(set-square (set-square s keeperRow keeperCol newKeeperContent) newKeeperRow newKeeperCol newEmptyContent))s
							)
							(if
								(or (isBox contentOfNewPosition) (isBoxStar contentOfNewPosition))
								(set-square beforeBoxMoveState newBoxRow newBoxCol (if (isBlank contentOfNewBoxPosition) box boxstar))
								beforeBoxMoveState
							)
						)
					)
				)
			)
		)
	)
)



(defun is-move-valid (keeperType newKeeperPositionContent potentialBoxPositionContent)
	(cond
		((null newKeeperPositionContent)		nil)
		((isWall newKeeperPositionContent)		nil)
		((or (isBox newKeeperPositionContent) (isBoxStar newKeeperPositionContent))
			(not
				(or
					(null potentialBoxPositionContent)
					(isBox potentialBoxPositionContent)
					(isWall potentialBoxPositionContent)
					(isBoxStar potentialBoxPositionContent)					
				)
			)
		)
		(t										t)
	)
)

;used to test problems
(defun tester (s )
	(printStates (a* s #'goal-test #'next-states #'hUID) 0.2)
)



; This is the trivial admissible heursistic that returns the value of zero.
; Number of moves required to win is at least 0 (if current state = winning state)
; That makes this heuristic addmissable
;
; s - the state of a board (not needed, added for symmetry with other heuristic functions)
(defun h0 (s)
	0
)

; This function breaks down the state s into rows and calls the h1Row method to
; calcualte the independent count of rows and then adds then together and returns
; that value
;
; s - The state of a board
(defun h1 (s)
	(cond
		((null s)			0)
		(t					(+ (h1Row (first s)) (h1 (rest s))))
	)
)

; This function goes through every row and counts how many items
; of type box are in this row.

; sRow - A single row from a state s
(defun h1Row (sRow)
	(cond
		((null sRow)				0)
		((isBox (first sRow))		(+ 1 (h1Row (rest sRow))))
		(t							(h1Row (rest sRow)))
	)
)

; returns a list of coordinates for all objects of type "V" in s
; should be called in with a 0 originally for currentRow
(defun getLocationOfVType (s V currentRow)
	(cond
		((null s)				nil)
		(t						(append (getLocationOfVType-Row (first s) V currentRow 0) (getLocationOfVType (rest s) V (+ currentRow 1))))
	)
)

(defun getLocationOfVType-Row (sRow V currentRow currentCol)
	(cond
		((null sRow)			nil)
		((= v (first sRow))		(append (list (list currentRow currentCol)) (getLocationOfVType-Row (rest sRow) V currentRow (+ currentCol 1))))
		(t						(getLocationOfVType-Row (rest sRow) V currentRow (+ currentCol 1)))
	)
)

; given two coordinates it returns the eucldian distance between them
(defun ManhattanDistance (coord1 coord2)
	(let*
		(
			(r1		(first coord1))
			(c1		(second coord1))
			(r2		(first coord2))
			(c2		(second coord2))
			(rowD	(- r2 r1))
			(colD	(- c2 c1))
		)
		(+ (myAbs rowD) (myAbs colD))	
	)
)

; calculates the absolute value of a number
(defun myAbs (n)
	(if
		(< n 0)
		(* n -1)
		n
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;
; This function adds up the manhattan distances of all the boxes to their
; nearest goal location
(defun hUID (s)
	(let*
		(
			(boxes			(getLocationOfVType s box 0))
			(goals			(getLocationOfVType s star 0))
		)
		(minimizeDistanceFunction boxes goals)
	)
	;(h1 s)
)

(defun minimizeDistanceFunction	(boxes goals)
	(cond
		((null boxes)				0)
		((null (rest boxes))		(minimizeSingleCoordinate (first boxes) goals))
		(t							(+ (minimizeSingleCoordinate (first boxes) goals) (minimizeDistanceFunction (rest boxes) goals)))
	)
)

(defun minimizeSingleCoordinate (coord possibleCoordPairs)
	(cond
		((null possibleCoordPairs)				0)
		((null (rest possibleCoordPairs))		(ManhattanDistance coord (first possibleCoordPairs)))
		(t										(myMin (ManhattanDistance coord (first possibleCoordPairs)) (minimizeSingleCoordinate coord (rest possibleCoordPairs))))
	)
)


(defun myMin (val1 val2)
	(if
		(< val1 val2)
		val1
		val2
	)
)

(defun testMinDistance (s)
	(minimizeDistanceFunction (getLocationOfVType s box 0) (getLocationOfVType s star 0))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
