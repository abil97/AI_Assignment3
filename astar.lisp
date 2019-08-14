(setq maze '( (("Arad" 0 0) (("Zerind" 75) ("Timisoara" 118) ("Sibiu" 140)))
				(("Bucharest" 0 0) (("Fagaras" 211) ("Giurgiu" 90) ("Pitesti" 101) ("Urziceni" 85)))
				(("Craiova" 0 0) (("Dobreta" 120) ("Rimnicu Vilcea" 146) ("Pitesti" 138)))
				(("Dobreta" 0 0) (("Mehadia" 75) ("Craiova" 120)))
				(("Eforie" 0 0) (("Hirsova" 86)))
				(("Fagaras" 0 0) (("Sibiu" 99) ("Bucharest" 211))) 
				(("Giurgiu" 0 0) (("Bucharest" 90)))
				(("Hirsova" 0 0) (("Urziceni" 98) ("Eforie" 86)))
				(("Iasi" 0 0) (("Vaslui" 92) ("Neamt" 87)))
				(("Lugoj" 0 0) (("Timisoara" 111) ("Mehadia" 70)))
				(("Mehadia" 0 0) (("Lugoj" 70) ("Dobreta" 75)))
				(("Neamt" 0 0) (("Iasi" 87)))
				(("Oradea" 0 0) (("Zerind" 71) ("Sibiu" 151)))
				(("Pitesti" 0 0) (("Rimnicu Vilcea" 97) ("Craiova" 138) ("Bucharest" 101))) 
				(("Rimnicu Vilcea" 0 0) (("Sibiu" 80) ("Craiova" 146) ("Pitesti" 97)))
				(("Sibiu" 0 0) (("Oradea" 151) ("Rimnicu Vilcea" 80) ("Fagaras" 99) ("Arad" 140)))
				(("Timisoara" 0 0) (("Lugoj" 70) ("Arad" 118)))
				(("Urziceni" 0 0) (("Bucharest" 85) ("Hirsova" 98) ("Vaslui" 142))) 
				(("Vaslui" 0 0) (("Iasi" 87) ("Urziceni" 142))) 
				(("Zerind" 0 0) (("Arad" 75) ("Oradea" 71)))	

			)	)
				;("Name" d d+L) ---> d - the actual distance traveled to reach this node
;									d+L - d + linear distance to goal

(setf cityList '("Arad" "Bucharest" "Craiova" "Dobreta" "Eforie" "Fagaras" "Giurgiu" "Hirsova" "Iasi" "Lugoj" "Mehadia" "Neamt" "Oradea"
					"Pitesti" "Rimnicu Vilcea" "Sibiu" "Timisoara" "Urziceni" "Vaslui" "Zerind"))
(setq distances-from-Bucharest '( ("Arad" 366)
								  ("Bucharest" 0)
								  ("Craiova" 160)
								  ("Dobreta" 242)
								  ("Eforie" 161)
								  ("Fagaras" 178)
								  ("Giurgiu" 77)
								  ("Hirsova" 151)
								  ("Iasi" 226)
								  ("Lugoj" 244)
								  ("Mehadia" 241)
								  ("Neamt" 234)
								  ("Oradea" 380)
								  ("Pitesti" 98)
								  ("Rimnicu Vilcea" 193)
								  ("Sibiu" 253)
								  ("Timisoara" 329)
								  ("Urziceni" 80)
								  ("Vaslui" 199)
								  ("Zerind" 374)

	))
; 				
(setf frontier '())
(setf closedList '())
(setf new-frontier '())
(setf new-closed-list '())
(setf ggoal '())


; Main search function
(defun a-star-search(start goal maze)
	(setf start-node '())
	(setf goal-node '())
	(setf ggoal goal)

	; Find a nodes for starting and goal cities
	(dolist (el maze)
		(if (string-equal (caar el) start)
			(setf start-node el)
		)
		(if (string-equal (caar el) goal)
			(setf goal-node el)
		)
	)
	
	(setf frontier (add-to-tail frontier start-node))
	 (findRoute goal-node maze)
	;(print start-node)
	;(print goal-node)
	(setf tmp '())
	(setf tmp (add-to-tail tmp start))
	(setf tmp (add-to-tail tmp (heuristic start-node goal-node)))
	(setf new-closed-list (reverse (add-to-tail (reverse new-closed-list) tmp)))

	(setf lst (helper closedList))
	(write-line "=====================================================")
	(write-line "PATH IS")
	(dolist (itm new-closed-list)
		(write itm)
		(write-char #\Space)
	)
	(terpri)
	(write-line "--------------------------------")
	(write-line "FRINGE IS")
	(dolist (itm lst)
		(write itm)
		(write-char #\Space)
	)
	
)

; goal is FULL goal node
; Recursive function that finds route
(defun findRoute(goal maze)

	(if (not (null frontier))
		
	(progn

	(setf curr-node (car frontier))
	(setf frontier (cdr frontier))
	(setf new-frontier (cdr new-frontier))

	(if (string-equal (caar curr-node) (caar goal))
		(progn
		;(setf closedList (add-to-tail closedList (caar curr-node)))
		(return-from findRoute (caar curr-node)))
	)

	; If location is not on the closedList
	(if (= (contains closedList (caar curr-node)) 0)
		(progn 
			(setf closedList (add-to-tail closedList (caar curr-node)))
			(addChildrenToFrontier curr-node goal)
		)
	)
	(setf res (findRoute goal maze))
	
)))


; Function that adds children to frontier
(defun addChildrenToFrontier(ParentNode GoalNode)

	; child is list of child nodes -->  (("Oradea" 151) ("Rimnicu Vilcea" 80) ("Fagaras" 99) ("Arad" 140))
	; child list IS IN THE ParentNode
	(setf minlist '())
	(dolist (child (cadr ParentNode))

		(setf childCity (car child)) ; ChildCity name is just the name of the city
		(setf distBetweenParentAndChild (cadr child)) ; Distance between child and parent

		(if (= (contains closedList childCity) 0)
			(progn
				; Searching FULL child node
				(setf child-node '())
				(dolist (el maze)
					(setf node (car el))
					(if (string-equal (car node) childCity)
						(setf child-node el)
					)
				)
				(setf old-child-node child-node)
				(setf totalDist 0) ; TotalDist is f, f = g + h
				(setf parentDist (elt (car ParentNode) 1))   ; Total distance to reach parent
				(setf childDist (+ parentDist distBetweenParentAndChild)) ; Total distance to reach child

				(setf h (heuristic child-node GoalNode))
				(setf totalDist (+ h childDist)) ; Total cost

				(setf childCar (car child-node))	; Car of the FULL child node
				(setf (elt childCar 1) childDist)	; Insert childDist instead of d --> ("faragas" d d+L)
				(setf (elt childCar 2) totalDist)	; Insert totalDist instead of d + L --> ("faragas" d d+L)
				(setf (elt child-node 0) childCar)	; Insert updated child car back to child-node
				(setf maze (substit old-child-node child-node))
				
				(setf minlist (add-to-tail minlist child-node))
				(setf minlist (sort minlist #'< :key #'caddar))
				(setf tmp '())
				(setf tmp (add-to-tail tmp (caar child-node)))
				(setf tmp (add-to-tail tmp (elt (car child-node) 2)))
				(setf new-frontier (add-to-tail new-frontier tmp))
				(setf new-frontier (sort new-frontier #'< :key #'cadr))
			)
		)
	)
	(setf frontier (add-to-tail frontier (car minlist)))
	(setf frontier (sort frontier #'< :key #'caddar))

	(setf tmp '())
	(setf tmp (add-to-tail tmp (caaar minlist)))
	(setf tmp (add-to-tail tmp (elt (caar minlist) 2)))
	(setf new-closed-list (add-to-tail new-closed-list tmp))
	;(setf new-closed-list (reverse new-closed-list))
	;(setf finlist (add-to-tail finlist (caaar minlist)))
	(setf minlist '())
)





; Heuristic function
(defun heuristic(a b)

	(setf cityA (caar a))
	(setf cityB (caar b))

	(dolist (el distances-from-Bucharest)
		(if (string-equal (car el) cityA)
			(setf disA (cadr el))
		)
		(if (string-equal (car el) cityB)
			(setf disB (cadr el))
		)

	)
	(setf h (abs (- disA disB)))
	h

)

; Check is element is in the list 
(defun contains(lst el)
	(dolist (tmp lst)
		(if (string-equal el tmp)
			(return-from contains 1)

		)
	)
	(return-from contains 0)
)


; Helper function that adds element tothe tail of the list
; Taken from source https://stackoverflow.com/a/13367974/9901274
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)


(defun substit(old new)
	(setf new-maze '())
	(dolist (el maze)
		(if (string-equal (caar old) (caar el))
			(setf new-maze (add-to-tail new-maze new))
			(setf new-maze (add-to-tail new-maze el))
		)
	)
	new-maze
)

(defun helper(lst)
	(setf childrens '())
	(setf fnlist '())
	(dolist (item lst)
		(dolist (el maze)
			(if (string-equal item (caar el))
				(progn 
					(dolist (x (cadr el))
						(setf childrens (add-to-tail childrens (car x))))))
		)
	)
	(dolist (item lst)
		(if (= (contains childrens item) 1)
			(setf childrens (remove item childrens :test #'equal)))
	)
	(setf childrens (remove-duplicates childrens :test #'equal))
	(setf childrens (remove gg childrens :test #'equal))

	(dolist (item childrens)
		(dolist (el maze)
			(if (string-equal item (caar el))
				(progn
					(setf tmp '())
					(setf tmp (add-to-tail tmp item))
					(setf tmp (add-to-tail tmp (caddar el)))
					(setf fnlist (add-to-tail fnlist tmp))))
		)
	)
	(return-from helper (sort fnlist #'< :key #'cadr))
)


(write-line "Enter the starting city: ")
(setf st (string-capitalize (string-downcase (string (read)))))
(if (or (= (contains cityList st) 0))
	(progn
	(write-line "This city does not exist. Check correctness of your input. Try again")
	(quit)
))
(write-line "Enter the target city: ")
(setf gg (string-capitalize (string-downcase (string (read)))))
(if (or (= (contains cityList gg) 0))
	(progn
	(write-line "This city does not exist. Check correctness of your input. Try again")
	(quit)
))

(a-star-search st gg maze)
