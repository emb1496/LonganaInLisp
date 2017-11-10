#||
	 ************************************************************
     * Name:  Elliott Barinberg                                 *
     * Project: Longana project 2					            *
     * Class:  OPL								                *
     * Date:  10/24/17						                    *
     ************************************************************
||#


;;FUNCTIONS TO CREATE GAMESTATE BEGIN HERE


#||
Function Name: create_tiles
Purpose: To create a list of all the tiles for the game longana
Parameters:
	left an atom representing the left side of the new tile
	right an atom representing the right side of the new tile
	tiles a list of tiles already created
Return value
	list of the tiles 0-0 through 6-6 with no left right repition
Algorithm
	Check if left is 6:
		if true then do:
			check if right is six:
				if true then add (6 . 6) to the list and return final list
				else recursively call create_tiles with right+1 right+1 and the list containing the now added tile (left . right)
		else call create tiles with left+1 right and the list containing the now added tile (left . right)
||#
(defun create_tiles (left right tiles)
	(cond  																					
		((equal left 6) 																	
			(cond 																			
				((equal right 6)									 						
					(cons (cons 6 6) tiles))												
				(t 																			
																							
					(create_tiles (+ right 1) (+ right 1) (cons (cons left right) tiles))))) 
		(t 																					
			(create_tiles (+ left 1) right (cons (cons left right) tiles)))))				

#||
Function Name: create_player_hand
Purpose: To fill the player hand from a given list tiles and return it as a list
Parameters:
	count an atom representing the number of tiles to grab
	tiles a list of tiles to grab from
	hand is a list of tiles representing the player hand
Return value
	list of the first count tiles in tiles
Algorithm
	Check if left is 1:
		if true then do:
			return hand and the first element of tiles cons'd together
		else
			call create_player_hand with count-1, rest of tiles, and the hand which is the hand and first atom in tiles cons'd together
||#
(defun create_player_hand (count tiles hand)
	(cond 																				
		((equal count 1)																
			(cons (first tiles) hand))													
		(t 																				
			(create_player_hand (- count 1) (rest tiles) (cons (first tiles) hand)))))																				

#||
Function Name: remove_tiles
Purpose: To remove count number of tiles from tiles and return tiles
Parameters:
	count an atom representing the number of tiles to remove
	tiles a list of tiles to remove from
Return value
	list of tiles missing the first count number of tiles
Algorithm
	Check if count is 1:
		if true then do:
			return rest of tiles
		else
			call remove_tiles with count-1, rest of tiles
||#
(defun remove_tiles (count tiles)
	(cond
		((equal count 1)
			(rest tiles))
		(t
			(remove_tiles (- count 1) (rest tiles)))))

#||
Function Name: get_tile_at_n
Purpose: To return the tile at index n
Parameters:
	n is an atom representing the index
	tiles a list of tiles
Return value
	one tile (list of 2 elements)
Algorithm
	Check if count is 1:
		if true then do:
			return first tiles
		else
			call get_tile_at_n with count-1, rest of tiles
||#
(defun get_tile_at_n (n tiles)
	(cond
		((equal n 1)
			(first tiles))
		(t
			(get_tile_at_n (- n 1) (rest tiles)))))	

#||
Function Name: remove_tile_at_n
Purpose: To return the list without the index n
Parameters:
	n is an atom representing the index
	tiles a list of tiles to remove from
Return value
	tiles sans one tile
Algorithm
	Check if n is 1:
		if true then do:
			return rest tiles
		else
			cons first tile with (remove_tile_at_n n-1 rest tiles)
||#
(defun remove_tile_at_n (n tiles)
	(cond
		((equal n 1)
			(rest tiles))
		(t
			(cons (first tiles) (remove_tile_at_n (- n 1) (rest tiles))))))																						

#||
Function Name: m_length
Purpose: To return the length of the list
Parameters:
	tiles a list of tiles to count
Return value
	length
Algorithm
	Check if tiles is empty:
		if true then do:
			return 0
		else
			return (1 + (m_length (rest tiles)))
||#
(defun m_length (tiles)
	(cond
		((null tiles)
			0)
		(t
			(+ 1 (m_length (rest tiles))))))

#||
Function Name: shuffle_tiles
Purpose: To shuffle and return tiles
Parameters:
	tiles a list of tiles to remove from
Return value
	list of shuffled tiles
Algorithm
	1. Check if tiles is empty, if so return the list empty (which at that point is full of what we need)
	2. If the tiles are not empty, seed the random number generator
	3. Generate a random number from 0 to the length of tiles and add 1
	4. Save the tile at that index to tile
	5. Call shuffle tiles with tiles missing the tile at that index and empty containing it
||#
(defun shuffle_tiles (tiles empty)
	(cond
		((null tiles)
			empty)
		(t
			(let* ( 	(state (make-random-state t))
						(number (random (m_length tiles) state))
						(number (+ number 1))
						(tile (get_tile_at_n number tiles)))
				(shuffle_tiles (remove_tile_at_n number tiles)(cons tile empty))))))

#||
Function Name: get_engine
Purpose: To return center tile
Parameters:
	numrounds is an atom representing the number of rounds
Return value
	engine tilee
Algorithm
	1. Check if numrounds mod 7 equals 0
	2. If so the center tile is 0-0 we cons it and return it
	3. Else
	4. We cons (7 - (numrounds mod 7)) with itself and return that tile
||#
(defun get_engine (numrounds)
	(cond
		((equal 0 (mod numrounds 7))
			(cons 0 0))
		(t
			(cons (- 7 (mod numrounds 7)) (- 7 (mod numrounds 7))))))

#||
Function Name: add_center
Purpose: add the center tile to the board
Parameters:
	board a list of tiles to represent board
	tile is an atom in board but a list of 2 numbers itself
Return value
	list of tiles
Algorithm
	1. Check if board is empty
		2. If so then return cons board tile
	3. Otherwise
		4. Return the original board as the board already had tiles in it
||#
(defun add_center (tile board)
	(cond
		((null board)
			(cons tile board))
		(t
			board)))

#||
Function Name: get_score
Purpose: to read in the tournament score from the user
Parameters:
	none
Return value
	score
Algorithm
	1. Prompt user for score
	2. Check that score is greater than 0, if true return score
	3. If false call get score again
||#
(defun get_score ()
	(princ "Please enter tournament score: ")
	(let* (
			(score (read)))
		(cond
			((numberp score)
				(cond
					((> score 0)
						score)))
			(t
				(get_score)))))

#||
Function Name: has_tile
Purpose: to check if a hand contains a tile
Parameters:
	tile -> the tile as a list to check for
	hand -> the list of tiles to checl
Return value
	nil or t depending on whether or not the user has the tile
Algorithm
	1. Check if hand is empty, if so return nil
	2. Check if the first element in hand is equal to tile, if so return t
	2. Else, call has_tile with the tile and the hand sans the first element
||#
(defun has_tile (tile hand)
	(cond
		((null hand)
			nil)
		((equal tile (first hand))
			t)
		(t
			(has_tile tile (rest hand)))))

#||
Function Name: remove_tile_from_hand
Purpose: to remove a tile from hand
Parameters:
	tile -> the tile as a list to remove
	hand -> the list of tiles representing the player hand
Return value
	hand sans tile
Algorithm
	1. Check if the first tile in hand is the tile we are trying to remove, if so return the rest of hand
	2. Else return the first element in hand combined with the call to remove_tile_from_hand using the tile and the rest of hand
||#
(defun remove_tile_from_hand (tile hand)
	(cond
		((equal tile (first hand))
			(rest hand))
		(t
			(cons (first hand) (remove_tile_from_hand tile (rest hand))))))

#||
Function Name: print_hand_index
Purpose: to print the index in the right spots to line up with the tiles
Parameters:
	hand --> list of tiles
	start --> atomic counter value
Return value
	none
Algorithm
	1. Check if hand is empty, if so stop printing
	2. Check if the start counter has gone above single digits, if so print the right number of spaces and the counter value accordingly then call print_hand_index with the rest of hand and an incremented counter
	3. Otherwise print the number of spaces to a single digit number, and then call print_hand_index with the rest of hand and the incremented counter
||#
(defun print_hand_index (hand start)
	(cond
		((null hand))
		((>= start 10)
			(princ "    ")
			(princ start)
			(princ "  ")
			(print_hand_index (rest hand) (+ start 1)))
		(t
			(princ "    ")
			(princ start)
			(princ "   ")
			(print_hand_index (rest hand) (+ start 1)))))

#||
Function Name: valid
Purpose: To validate the user input
Parameters:
	h_length --> length of hand
	new --> user input
Return value
	user input
Algorithm
	1. Check if the human entered an integer or a symbol
	2. If it is an integer it checks to make sure that the input was between one and h_length
	3. If it is a symbol make sure it is one of the predetermined symbols
	4. Return new
||#
(defun valid (h_length new)
	(cond
		((integerp new)
			(cond
				((> new h_length)
					(princ "Invalid entry")
					(terpri)
					(get_index h_length))
				((< new 1)
					(princ "Invalid entry")
					(terpri)
					(get_index h_length))
				(t
					new)))
		((symbolp new)
			(cond
				((equal new 'p)
					new)
				((equal new 'h)
					new)
				((equal new 's)
					new)
				(t
					(princ "Invalid entry")
					(terpri)
					(get_index h_length))))
		(t
			(princ "Invalid entry")
			(terpri)
			(get_index h_length))))

#||
Function Name: get_index
Purpose: to get the human input
Parameters:
	h_length --> hand size
Return value
	validated input
Algorithm
	1. Ask user for the input
	2. Call valid with the user input
||#
(defun get_index (h_length)
	(princ "Please enter which tile you would like to move 1 - ") (princ h_length) (princ " or enter 'p' to pass, 's' to save, or 'h' for help: ")
	(valid h_length (read)))




;;;		FUNCITONS FOR PRINTING GAMESTATE BEGIN HERE





#||
Function Name: count_doubles_index
Purpose: to count the index where the doubles are
Parameters:
	board --> list of tiles being the board
	count --> atomic counter value
Return value
	list of double's positions
Algorithm
	1. Check if board is empty
		if so return nil
	2. Check if the first tile in board is a double
		if so cons the current count with the recursive count_doubles_index with rest of the board
	3. Otherwise
		call count_doubles_index with the rest of the board and increment count
||#
(defun count_doubles_index (board count)
	(cond
		((null board)
			NIL)
		((equal (first (first board)) (rest (first board)))
			(cons count (count_doubles_index (rest board) (+ count 1))))
		(t
			(count_doubles_index (rest board) (+ count 1)))))

#||
Function Name: print_first_line_of_board
Purpose: to print one pip of the doubles to the console
Parameters:
	index_of_doubles --> list of doubles indexes
	board --> list of tiles representing board
	count --> atomic counter
Return value
	none - displays to screen
Algorithm
	1. Check if there are no more doubles, if so print a line and return
	2. Check if there are no tiles in board, if so print a line and return
	3. Check if the length of board is one, if so print that engine pip
	4. Check if the count has reached an index of a double, if so print the pip at that spot and make the recursive call with the rest of index of doubles and board and incremented count
	5. Otherwise print the blank spot and do the recursive call with the index of doubles the rest of board and the incremented count 
||#
(defun print_first_line_of_board (index_of_doubles board count)
	(cond
		((null index_of_doubles)
			(terpri))
		((null board)
			(terpri))
		((equal (m_length board) 1)
			(princ " ")
			(princ (first (first board)))
			(princ " ")
			(terpri))
		((equal count (first index_of_doubles)) 
			(princ " ")
			(princ (first (first board)))
			(print_first_line_of_board (rest index_of_doubles) (rest board) (+ count 1)))
		(t
			(princ "    ")
			(print_first_line_of_board index_of_doubles (rest board) (+ count 1)))))

#||
Function Name: print_second_line_of_board
Purpose: to output the middle line of the board to the screen
Parameters:
	index_of_doubles --> list of doubles indexes
	board --> list of tiles representing the board
	count --> atomic counter
Return value
	none - prints line to console
Algorithm
	1. Check if board is empty, if so print the R representing right side
	2. Check if there are no more doubles if so print the next tile and recursively print the rest
	3. Check if the counter has reached the next double and print a | as the place holder
	4. Otherwise print the tile and recursively call the function with the rest of the baord
||#
(defun print_second_line_of_board (index_of_doubles board count)
	(cond
		((null board)
			(princ "  R")
			(terpri))
		((null index_of_doubles)
			(princ " ")
			(princ (first (first board)))
			(princ "-")
			(princ (rest (first board)))
			(print_second_line_of_board nil (rest board) (+ count 1)))
		((equal count (first index_of_doubles))
			(princ " |")
			(print_second_line_of_board (rest index_of_doubles) (rest board) (+ count 1)))
		(t
			(princ " ")
			(princ (first (first board)))
			(princ "-")
			(princ (rest (first board)))
			(print_second_line_of_board index_of_doubles (rest board) (+ count 1)))))

#||
Function Name: print_board
Purpose: to print the board to the screen in the proper format
Parameters:
	board --> list of tiles representing the board
Return value
	none - ouputted board
Algorithm
	1. Create an index of doubles
	2. Print the first line of the board using print_first_line_of_board
	3. Print the middle line of board using print_second_line_of_board
	4. Print the last line of the board using print_first_line_of_board
||#
(defun print_board (board)
	(let* ( (index_of_doubles ())
			(index_of_doubles (count_doubles_index board 1)))
		(terpri)
		(princ "  ")
		(print_first_line_of_board index_of_doubles board 1)
		(princ "L ")
		(print_second_line_of_board index_of_doubles board 1)
		(princ "  ")
		(print_first_line_of_board index_of_doubles board 1)))



;;; FUNCTIONS FOR THE HUMAN TO PLAY TILES BEGIN HERE



#||
Function Name: get_right_most
Purpose: to return the right most pip
Parameters:
	board --> list of tiles
Return value
	last atomic value in the list
Algorithm
	1. Recusively call get_right_most with the rest of the board until the length is only one
	2. Return the right most pip of the last tile
||#
(defun get_right_most (board) ; return right most pip
	(cond
		((equal (m_length board) 1)
			(rest (first board)))
		(t
			(get_right_most (rest board)))))

#||
Function Name: can_play_tile
Purpose: to determine if a particular tile can be played
Parameters:
	tile --> a tile to be played (list of 2 atoms)
	board --> list of tiles
	comp_passed --> a value (t/nil) to determine if the last player passed
Return value
	t or nil
Algorithm
	1. Set the values for left right and left and right most pips
	2. check if the tile can be played and return the appropriate value
||#
(defun can_play_tile (tile board comp_passed) ; function to determine if a particular tile can be played, this function will be used directly in human attempt
	(let* ((left_most_pip (first (first board)))
		(right_most_pip (get_right_most board))
		(left (first tile))
		(right (rest tile)))
		(cond
			((equal left left_most_pip)
				t)
			((equal right left_most_pip)
				t)
			((equal comp_passed t)
				(cond
					((equal left right_most_pip)
						t)
					((equal right right_most_pip)
						t)))
			((equal left right)
				(cond
					((equal left right_most_pip)
						t)
					((equal right_most_pip right)
						t)))
			(t
				nil))))

#||
Function Name: has_legal_move
Purpose: to determine if the human has a legal move
Parameters:
	human --> list of tiles representing player hand
	board --> list of tiles representing the board
	comp_passed --> (t/nil) representing whether last player passed
Return value
	t or nil
Algorithm
	1. check if the human hand is empty, if so return nil
	2. Otherwise call can_play_tile with the first tile in human board and comp passed, if t return t
	3. Recursively call has_legal_move with the rest of human, board, and comp passed
||#
(defun has_legal_move (human board comp_passed) ; this function will determine if human has a legal move and return t for true
	(cond
		((null human)
			nil)
		(t
			(cond
				((equal (can_play_tile (first human) board comp_passed) t)
					t)
				(t
					(has_legal_move (rest human) board comp_passed))))))

#||
Function Name: valid_side
Purpose: to validate side input
Parameters:
	input --> human input
Return value
	input, once validated
Algorithm
	1. Check if input is 'l' or 'r', if so return input
	2. Otherwise call get_side 
||#
(defun valid_side (input)
	(cond
		((symbolp input)
			(cond
				((equal input 'l)
					input)
				((equal input 'r)
					input)
				(t
					(get_side))))
		(t
			(get_side))))

#||
Function Name: get_side
Purpose: to have the user enter which side they want to input to
Parameters:
	none
Return value
	validated input
Algorithm
	1. Prompt user for which side they want to play on
	2. Call valid_side with the user input
||#
(defun get_side ()
	(princ "Enter which side (l/r): ")
	(valid_side (read)))

#||
Function Name: play_it
Purpose: to attach the tile to the board
Parameters:
	tile --> the tile to play
	board --> list of tiles
Return value
	the updated board
Algorithm
	1. Check which side the user wants to play
	2. Check if tile can be played on that side
	3. If so play tile otherwise return board without tile
||#
(defun play_it (tile board)
	(let* ( (left_most_pip (first (first board)))
			(right_most_pip (get_right_most board))
			(left (first tile))
			(right (rest tile))
			(side (get_side)))
		(cond
			((equal side 'l)
				(cond
					((equal left_most_pip right)
						(cons (cons left right) board))
					((equal left_most_pip left)
						(cons (cons right left) board))
					(t
						board)))
			(t
				(cond
					((equal right_most_pip left)
						(append board (list (cons left right))))
					((equal right_most_pip right)
						(append board (list (cons right left))))
					(t
						board))))))




;;; THIS IS A GAMEPLAY FUNCTION FOR WHEN THE USER PASSED



#||
Function Name: just_passed
Purpose: to let the user make their play after passing
Parameters:
	board --> list of tiles
	human --> list of tiles
	stock --> list of tiles
	temp --> atomic value to use when needed
	comp_passed --> whether the computer passed(t/nil)
	computer --> list of tiles
	numrounds --> atomic value for number of rounds
	human_score --> atomic value for score
	computer_score --> atomic value for score
	score --> atomic value for tournament score
Return value
	no return, it will call the play_round function when done with updated hands and board
Algorithm
	1. Print the board
	2. Print the player hand and index
	3. Read in the user choice 1-hand length or 'p'/'h'
	4. Check if the input is a letter, if so call help or pass functionality
	5. Else call play it for tile which was chosen
	6. Check if the board changed, if so call play_round and continue
	7. Else tell the user it was invalid move and restart just_passed
||#
(defun just_passed (board human stock temp comp_passed computer numrounds human_score computer_score score)
	(print_board board)
	(print human)
	(terpri)
	(print_hand_index human 1)
	(terpri)
	(let* ( (temp (get_index (m_length human))))
		(cond
			((symbolp temp)
				(cond
					((equal temp 'p) ; pass
						(cond
							((has_legal_move human board comp_passed)
								(princ "You have a legal move, press 'h' for help")
								(just_passed board human stock temp comp_passed computer numrounds human_score computer_score score))
							(t
								(play_round board human computer stock 'c temp comp_passed t numrounds human_score computer_score score))))
					((equal temp 'h) ; help
						(just_passed_help board human computer stock temp comp_passed numrounds human_score computer_score score))
					(t ;; there is no way to save mid turn
						(princ "Invalid entry")
						(terpri)
						(just_passed board human stock temp comp_passed computer numrounds human_score computer_score score))))
			(t
				(cond
					((has_legal_move human board comp_passed)
						(let* ( (m_new_board (play_it (get_tile_at_n temp human) board)))
							(cond
								((equal m_new_board board)
									(print "Invalid move")
									(play_round board human computer stock 'h temp comp_passed human_passed numrounds human_score computer_score score))
								(t
									(play_round m_new_board (remove_tile_at_n temp human) computer stock 'c temp comp_passed nil numrounds human_score computer_score score)))))
					(t
						;;trying to play with no legal move
						(print "That is not a legal move press 'h' for help")
						(just_passed board human stock temp comp_passed computer numrounds human_score computer_score score)))))))





;;; THIS IS WHERE THE COMPUTER AI FUNCTIONS BEGIN AS WELL AS THE COMPUTER PLAY FUNCTIONS





#||
Function Name: is_double
Purpose: to determine if a tile is double
Parameters:
	tile --> dotted pair
Return value
	t or nil
Algorithm
	1. Check if the left and right sides of the tile are equal, if so return t
	2. Else return nil
||#
(defun is_double (tile)
	(cond
		((equal (first tile) (rest tile))
			t)
		(t
			nil)))

#||
Function Name: weighttile
Purpose: to assign a weight to the tile
Parameters:
	tile --> dotted dair
Return value
	sum of tile pips
Algorithm
	1. Add the left and right sides and return the value
||#
(defun weightile (tile)
	(+ (first tile) (rest tile)))

#||
Function Name: is_better_tile
Purpose: to determine if a particular tile is better
Parameters:
	first_element_init --> a tile dotted pair
	first_element_hand --> a tile dottep pair
Return value
	t or nil
Algorithm
	1. If the both tiles are doubles check if the first one is heavier if so return t otherwise nil
	2. If one tile is a double and the other is not return t if its the first and nil if its the second
	3. If neigher is a double, if the first tile is heavier return t else return nil
||#
(defun is_better_tile (first_element_init first_element_hand)
	(cond
		((is_double first_element_init)
			(cond
				((is_double first_element_hand)
					(cond
						((> (weightile first_element_init) (weightile first_element_hand))
							t)
						(t
							nil)))
				(t
					t)))
		(t
			(cond
				((is_double first_element_hand)
					nil)
				(t
					(cond
						((> (weightile first_element_init) (weightile first_element_hand))
							t)
						(t
							nil)))))))

#||
Function Name: make_best_tile_order
Purpose: to return a list of tiles in the best play order
Parameters:
	init --> list to return
	hand --> list of player hands
Return value
	liast of dotted pairs
Algorithm
	1. If hand is empty return init
	2. If init is empty return make_best_tile_order with init as the first tile in hand and then the rest of init
	3. If the length of one of them is one check if that one is a better tile than the first of the other, if so put it in the beginning
		Else seperate the first element and call make_best_tile_order with the remaining tile and the rest of the other list
	4. If the length is more than one check if the first tile in init is better than the first in hand
		If so return make_best_tile_order with rest of init and the first tile in init being the first in hand
		Otherwise take first of hand + make_best_tile_order with init and rest of hand
||#
(defun make_best_tile_order (init hand)
	(cond
		((null hand)
			init)
		((null init)
			(cons (make_best_tile_order (first hand) (rest hand)) (first init)))
		((numberp (first hand))
			(cond
				((equal (is_better_tile (first init) hand) t)
					(cond
						((null (rest init))
							(list (first init) hand))
						(t
							(cons (first init) (make_best_tile_order (rest init) hand)))))
				(t
					(cons hand init))))
		((numberp (first init))
			(cond
				((equal (is_better_tile init (first hand)) t)
					(make_best_tile_order (list init (first hand)) (rest hand)))
				(t
					(make_best_tile_order (list (first hand) init) (rest hand)))))
		((>= (m_length init) 2)
			(cond
				((equal (is_better_tile (first init) (first hand)) t)
					(make_best_tile_order (make_best_tile_order init (first hand)) (rest hand)))
				(t
					(make_best_tile_order (cons (first hand) init) (rest hand)))))))

#||
Function Name: can_computer_play_tile
Purpose: to determine if the computer can play a particular tile
Parameters:
	tile --> dotted dair
	board --> list of dotted pairs
	human_passed --> t/nil
Return value
	t/nil
Algorithm
	1. Create local variables for the left and right side of the tiles and the left and right side of the board
	2. If human passed check if the tile can be played on either side and return t or nil
	3. Otherwise check if the tile is a double, if check if it can play on either side
	4. If not a double then check if it can be played on the right side and return t/nil
||#
(defun can_computer_play_tile (tile board human_passed)
	(let* ( (left (first tile))
			 (right (rest tile))
			 (left_most_pip (first (first board)))
			 (right_most_pip (get_right_most board)))
		(cond
			((equal human_passed t)
				(cond
					((equal left_most_pip left)
						t)
					((equal left_most_pip right)
						t)
					((equal right_most_pip left)
						t)
					((equal right_most_pip right)
						t)
					(t
						nil)))
			(t
				(cond
					((equal (is_double tile) t)
						(cond
							((equal left_most_pip left)
								t)
							((equal right_most_pip left)
								t)
							(t
								nil)))
					(t
						(cond
							((equal right_most_pip left)
								t)
							((equal right_most_pip right)
								t)
							(t
								nil))))))))

#||
Function Name: play_the_tile
Purpose: to attach the tile to the board
Parameters:
	tile --> dotted dair
	board --> list of dotted pairs
Return value
	tile as a part of board
Algorithm
	1. Create local variables for the left and right side of the tiles and the left and right side of the board
	2. Place the tile on the correct side of the board
||#
(defun play_the_tile (tile board)
	(let* ( (left_most_pip (first (first board)))
			(right_most_pip (get_right_most board))
			(left (first tile))
			(right (rest tile)))
			(cond
				((equal right_most_pip left)
					(princ " and because the computer strategy is to play on the right,")
					(terpri)
					(princ "the tile will be placed on the right side of the board")
					(terpri)
					(append board (list (cons left right))))
				((equal right_most_pip right)
					(princ " and because the computer strategy is to play on the right,")
					(terpri)
					(princ "the tile will be placed on the right side of the board")
					(terpri)
					(append board (list (cons right left))))
				((equal left_most_pip left)
					(princ " but it could not play it on the right so the computer placed it on the left.")
					(terpri)
					(cons (cons right left) board))
				((equal left_most_pip right)
					(princ " but it could not play it on the right so the computer placed it on the left")
					(terpri)
					(cons (cons left right) board)))))

#||
Function Name: computer_just_passed
Purpose: to have the computer make a move after passing
Parameters:
	hand --> list of dotted pairs
	computer --> list of dotted pairs
	board --> list of dotted pairs
	stock --> list of dotted pairss
	human_passed --> t/nil whether human passed
	human --> list of dotted pairs
	temp --> temporary variable
	numrounds --> atomic value for the number of rounds
	human_score --> human player score value
	computer_score --> computer player score value
	score --> tournament score
Return value
	calls play_round when done
Algorithm
	1. Create local variable for the best play order of the computer hand
	2. If the computer has no moves he passes again
	3. Otherwise he will try to play the first tile
	4. If possible it call play_round again
	5. Otherwise it will call computer_just_passed with the rest of hand
||#
(defun computer_just_passed (hand computer board stock human_passed human temp numrounds human_score computer_score score)
	(let* ( (best_tile_order (make_best_tile_order () hand)))
			(cond
				((null hand)
					(princ "After drawing, computer still has no legal moves, human to play")
					(terpri)
					(play_round board human computer stock 'h temp t human_passed numrounds human_score computer_score score))
				((equal (integerp (first (first best_tile_order))) t)
					(cond
						((equal (can_computer_play_tile (first best_tile_order) board human_passed) t)
							(princ "Computer ordered tiles in the order it would take to play them and ")
							(princ (first best_tile_order))
							(princ " was the best tile which could legally be moved")
							(play_round (play_the_tile (first best_tile_order) board) human (remove_tile_from_hand (first best_tile_order) computer) stock 'h temp nil human_passed numrounds human_score computer_score score))
						(t
							(princ "After drawing, computer still has no legal moves, human to play")
							(terpri)
							(play_round board human computer stock 'h temp t human_passed numrounds human_score computer_score score))))
				((equal (can_computer_play_tile (first (first best_tile_order)) board human_passed) t)
					(princ "After drawing, computer can play tile ")
					(princ (first (first best_tile_order)))
					(terpri)
					(play_round (play_the_tile (first (first best_tile_order)) board) human (remove_tile_from_hand (first (first best_tile_order)) computer) stock 'h temp nil human_passed numrounds human_score computer_score score))
				(t
					(computer_just_passed (rest (first best_tile_order)) computer board stock human_passed human temp numrounds human_score computer_score score)))))

#||
Function Name: computer_plays_move
Purpose: to make the computer's move
Parameters:
	hand --> list of dotted pairs
	computer --> list of dotted pairs
	board --> list of dotted pairs
	stock --> list of dotted pairss
	human_passed --> t/nil whether human passed
	human --> list of dotted pairs
	temp --> temporary variable
	numrounds --> atomic value for the number of rounds
	human_score --> human player score value
	computer_score --> computer player score value
	score --> tournament score
Return value
	calls play_round when done
Algorithm
	1. Create local variable for the best play order of the computer hand
	2. If the computer has no moves he passes to computer_just_passed
	3. Otherwise he will try to play the first tile
	4. If possible it call play_round again
	5. Otherwise it will call computer_plays_move with the rest of hand
||#
(defun computer_plays_move (hand computer board stock human_passed human temp numrounds human_score computer_score score)
	(let* ( (best_tile_order (make_best_tile_order () hand)))
			(cond
				((equal (integerp (first (first best_tile_order))) t)
					(cond
						((equal (can_computer_play_tile (first best_tile_order) board human_passed) t)
							(princ "Computer ordered tiles in the order it would take to play them and ")
							(princ (first best_tile_order))
							(princ " was the best tile which could legally be moved")
							(play_round (play_the_tile (first best_tile_order) board) human (remove_tile_from_hand (first best_tile_order) computer) stock 'h temp nil human_passed numrounds human_score computer_score score))
						(t
							(cond
								((null stock)
									(princ "Stock is empty, computer passes")
									(terpri)
									(play_round board human computer stock 'h temp t human_passed numrounds human_score computer_score score))
								(t
									(princ "Computer draws tile")
									(terpri)
									(computer_just_passed (create_player_hand 1 stock computer) (create_player_hand 1 stock computer) board (remove_tiles 1 stock) human_passed human temp numrounds human_score computer_score score))))))
				((equal (can_computer_play_tile (first (first best_tile_order)) board human_passed) t)
					(princ "Computer ordered tiles in the order it would like to play them and ")
					(princ (first (first best_tile_order)))
					(princ " was the best tile which could legally be moved")
					(play_round (play_the_tile (first (first best_tile_order)) board) human (remove_tile_from_hand (first (first best_tile_order)) computer) stock 'h temp nil human_passed numrounds human_score computer_score score))
				((null hand)
					(cond
						((null stock)
							(princ "Stock is empty, computer passes")
							(terpri)
							(play_round board human computer stock 'h temp t human_passed numrounds human_score computer_score score))
						(t
							(princ "Computer draws tile")
							(terpri)
							(computer_just_passed (create_player_hand 1 stock computer) (create_player_hand 1 stock computer) board (remove_tiles 1 stock) human_passed human temp numrounds human_score computer_score score))))
				(t
					(computer_plays_move (rest (first best_tile_order)) computer board stock human_passed human temp numrounds human_score computer_score score)))))




;;; THIS IS AN END OF ROUND FUNCTION



#||
Function Name: sumpips
Purpose: to return the sum of the pips in a hand
Parameters:
	hand --> list of dotted pairs
Return value
	sum of pips
Algorithm
	1. If hand is empty return 0
	2. Otherwise return the sum of the first tile plus the sumpips of rest of hand
||#
(defun sumpips (hand)
	(cond
		((null hand)
			0)
		(t
			(+ (+ (first (first hand)) (rest (first hand))) (sumpips (rest hand))))))






;;; THIS IS WHERE THE HELP FUNCTIONS BEGIN




#||
Function Name: help_can_play_tile
Purpose: to ive the user help
Parameters:
	tile --> dotted dair
	board --> list of dotted pairs
	comp_passed --> t/nil atomic value
Return value
	t/nil
Algorithm
	1. Create local variables for the left and right side of the tiles and the left and right side of the board
	2. Check if the tile is a double, if so give priority to the right side of the board
	3. Otherwise try to play it on the left, if it cannot be played on the left and the computer has passed then try to play it on the right
	4. Print out to the user when it is determined to be the best move
||#
(defun help_can_play_tile (tile board comp_passed) ; function to determine if a particular tile can be played, this function will be used directly in human attempt
	(let* ((left_most_pip (first (first board)))
		(right_most_pip (get_right_most board))
		(left (first tile))
		(right (rest tile)))
		(cond
			((equal left right)
				(cond
					((equal left right_most_pip)
						(princ "Because ") (princ tile) (princ " is a double it gets weighted higher than any uneven tiles. This is the heaviest double which can be legally played. ")
						(terpri)
						(princ "Therefore the computer recommends you moving ") (princ tile) (princ " to the right side, press enter to continue...")
						(read-line)
						(terpri)
						t)
					((equal left_most_pip right)
						(princ "Because ") (princ tile) (princ " is a double it gets weighted higher than any uneven tiles. This is the heaviest double which can be legally played. ")
						(terpri)
						(princ "Normally with doubles, we prefer to play on the right, however this double can only be played on the left.")
						(terpri)
						(princ "Therefore the computer recommends you moving ") (princ tile) (princ " to the left side, press enter to continue...")
						(read-line)
						(terpri)
						t)))
			((equal left left_most_pip)
				(princ "Because ") (princ tile) (princ " is the heaviest tile you can legally play on the left the computer recommends you moving ") (princ tile) (princ " to the left side, press enter to continue...")
				(read-line)
				(terpri)
				t)
			((equal right left_most_pip)
				(princ "Because ") (princ tile) (princ " is the heaviest tile you can legally play on the left the computer recommends you moving ") (princ tile) (princ " to the left side, press enter to continue...")
				(read-line)
				(terpri)
				t)
			((equal comp_passed t)
				(cond
					((equal left right_most_pip)
						(princ "Because ") (princ tile) (princ " is the heaviest tile you can legally play.")
						(terpri)
						(princ "And because you were unable to play any better tiles to the left the computer recommends you moving ") (princ tile) (princ " to the right side, press enter to continue...")
						(read-line)
						(terpri)
						t)
					((equal right right_most_pip)
						(princ "Because ") (princ tile) (princ " is the heaviest tile you can legally play.")
						(terpri)
						(princ "And because you were unable to play any better tiles to the left the computer recommends you moving ") (princ tile) (princ " to the right side, press enter to continue...")
						(read-line)
						(terpri)
						t)))			
			(t
				nil))))

#||
Function Name: help_has_legal_move
Purpose: to check if the player has a legal move to help with
Parameters:
	hand --> list of dotted pairs
	board --> list of dotted pairs
	comp_passed --> t/nil value
Return value
	t/nil
Algorithm
	1. If hand is empty then its nil
	2. Otherwise if the first tile can be played return t
	3. If not call help_has_legal_move with the rest of hand
||#
(defun help_has_legal_move (hand board comp_passed) ; this function will determine if human has a legal move and return t for true
	(cond
		((null hand)
			nil)
		((numberp (first (first hand)))
			(cond
				((equal (help_can_play_tile (first hand) board comp_passed) t)
					t)
				(t
					nil)))
		((equal (help_can_play_tile (first (first hand)) board comp_passed) t)
			t)
		(t
			(help_has_legal_move (list (rest (first hand))) board comp_passed))))

#||
Function Name: just_passed_help
Purpose: to help after the player has passed
Parameters:
	board --> list of dotted pairs
	human --> list of dotted pairs
	computer --> list of dotted pairs
	stock --> list of dotted pairs
	temp --> temporary value
	comp_passed --> t/nil
	numrounds --> atomic value
	human_score --> atomic value
	computer_score --> atomic value
	score --> atomic value
Return value
	call to just_passed
Algorithm
	1. If player has a legal move call help_has_legal_move
	2. Otherwise tell the user they have no legal moves
	3. Call just passed
||#
(defun just_passed_help (board human computer stock temp comp_passed numrounds human_score computer_score score)
	(cond
		((has_legal_move human board comp_passed)
			(help_has_legal_move (make_best_tile_order () human) board comp_passed)
			(just_passed board human stock temp comp_passed computer numrounds human_score computer_score score))
		(t
			(princ "You have no legal moves. Press 'p' to pass, press enter to continue...")
			(read-line)
			(terpri)
			(just_passed board human stock temp comp_passed computer numrounds human_score computer_score score))))

#||
Function Name: help
Purpose: to recommend a play for the user
Parameters:
	board --> list of dotted pairs
	human --> list of dotted pairs
	computer --> list of dotted pairs
	stock --> list of dotted pairs
	temp --> atomic value
	comp_passed --> t/nil
	human_passed --> t/nil
	numrounds --> atomic value
	human_score --> atomic value
	computer_score --> atomic value
	score --> atomic value
Return value
	call to play_round
Algorithm
	1. If the player has a legal move call help_has_legal_move then play round
	2. Otherwise tell user they have no moves and call play_round
||#
(defun help (board human computer stock temp comp_passed human_passed numrounds human_score computer_score score)
	(cond
		((has_legal_move human board comp_passed)
			(help_has_legal_move (make_best_tile_order () human) board comp_passed)
			(play_round board human computer stock 'h temp comp_passed human_passed numrounds human_score computer_score score))
		(t
			(princ "You have no legal moves. Press 'p' to pass, press enter to continue...")
			(read-line)
			(terpri)
			(play_round board human computer stock 'h temp comp_passed human_passed numrounds human_score computer_score score))))







;;; THIS IS WHERE THE MAIN GAMEPLAY FUNCTIONS BEGIN




#||
Function Name: play_round
Purpose: to play a round of moves
Parameters:
	board --> list of dotted pairs
	human --> list of dotted pairs
	computer --> list of dotted pairs
	stock --> list of dotted pairs
	play_next --> atomic value
	temp --> atomic value
	comp_passed --> t/nil
	human_passed --> t/nil
	numrounds --> atomic value
	human_score --> atomic value
	computer_score --> atomic value
	score --> atomic value
Return value
	call to play tournament
Algorithm
	1. Print the number of tiles left in the stock
	2. Check if either player has an empty hand and if so end the round and call play tournament
	3. Check if the round is over by the players both having passed and if so do the proper call to play tourn
	4. Otherwise check if human or computer plays next
	5. If it is the human prompt for character input, if numerical call appropriate functions to allow the user to play, if its alphabetical then call the proper functions there
	6. If it is the computer move, call the computer play function
||#
(defun play_round (board human computer stock play_next temp comp_passed human_passed numrounds human_score computer_score score)
	(cond
		((equal (m_length stock) 1)
			(princ "There is ") (princ (m_length stock)) (princ " tiles left in the boneyard"))
		(t
			(princ "There are ") (princ (m_length stock)) (princ " tiles left in the boneyard")))
	(terpri)
	(cond
		((null human)
			(let* (	(temp_human_score (sumpips human))
					(temp_computer_score (sumpips computer))
					(tiles (create_tiles 0 0 ()))
					(tiles (shuffle_tiles tiles ()))
					(board ())
					(temp ())
					(numrounds (+ numrounds 1))
					(center (get_engine numrounds))
					(board (add_center center board))
					(score score))
				(princ "Human player has empty hand, they win! Computer had ") 
				(princ temp_computer_score) (princ " remaining")
				(terpri)
				(princ "Human score is now ") 
				(princ (+ temp_computer_score human_score))
				(terpri)
				(princ "Computer score is now ") 
				(princ computer_score)
				(terpri)
				(princ "The tournament score is ") (princ score) 
				(terpri)
				(cond
					((equal (save_before_game) t)
						(save_to_file score numrounds (create_player_hand 8 tiles ()) computer_score (create_player_hand 8 (remove_tiles 8 tiles) ()) (+ temp_computer_score human_score) () (remove_tiles 16 tiles) () 'neither)
						(exit))
					(t
						(play_tournament board (create_player_hand 8 tiles ()) (create_player_hand 8 (remove_tiles 8 tiles) ()) (remove_tiles 16 tiles) numrounds center score (+ temp_computer_score human_score) computer_score temp)))))
		((null computer)
			(let* (	(temp_human_score (sumpips human))
					(temp_computer_score (sumpips computer))
					(tiles (create_tiles 0 0 ()))
					(tiles (shuffle_tiles tiles ()))
					(board ())
					(temp ())
					(numrounds (+ numrounds 1))
					(center (get_engine numrounds))
					(board (add_center center board))
					(score score))
				(princ "Computer player has empty hand, they win! Human had ") (princ temp_human_score) (princ " remaining")
				(terpri)
				(princ "Computer score is now ") (princ (+ temp_human_score computer_score))
				(terpri)
				(princ "Human score will remain ") (princ human_score)
				(terpri)
				(princ "The tournament score is ") (princ score) 
				(terpri)
				(cond
					((equal (save_before_game) t)
						(save_to_file score numrounds (create_player_hand 8 tiles ()) (+ temp_human_score computer_score) (create_player_hand 8 (remove_tiles 8 tiles) ()) human_score () (remove_tiles 16 tiles) () 'neither)
						(exit))
					(t
						(play_tournament board (create_player_hand 8 tiles ()) (create_player_hand 8 (remove_tiles 8 tiles) ()) (remove_tiles 16 tiles) numrounds center score human_score (+ temp_human_score computer_score) temp))))))
	(cond
		((null stock)
			(cond
				((equal comp_passed t)
					(cond
						((equal human_passed t)
							(let* ( 	(tiles (create_tiles 0 0 ())) ;; let* means its in order, then create_tiles 0 0 () is the initial call to the create tiles function
									 	(tiles (shuffle_tiles tiles ()))
		  								(temp_human_score (sumpips human))
		  	 							(temp_computer_score (sumpips computer))
		  	 							(board ())
		  	 							(temp ())
		  	 							(numrounds (+ numrounds 1))
		 								(center (get_engine numrounds))
		  	 							(board (add_center center board)))
								(cond
									((< temp_human_score temp_computer_score)
										(princ "Human wins with ") (princ temp_human_score) (princ " versus the computer's ") (princ temp_computer_score)
										(terpri)
										(princ "Human score is now ") (princ (+ temp_computer_score human_score))
										(terpri)
										(princ "Computer score will remain ")(princ computer_score)
										(terpri)
										(cond
											((equal (save_before_game) t)
												(save_to_file score numrounds (create_player_hand 8 tiles ()) computer_score (create_player_hand 8 (remove_tiles 8 tiles) ()) (+ temp_computer_score human_score) () (remove_tiles 16 tiles) () 'neither)
												(exit))
											(t
												(play_tournament board (create_player_hand 8 tiles ()) (create_player_hand 8 (remove_tiles 8 tiles) ()) (remove_tiles 16 tiles) numrounds center score (+ temp_computer_score human_score) computer_score temp)
											)))
									((< temp_computer_score temp_human_score)
										(princ "Computer wins with ") (princ temp_computer_score) (princ " versus the human's ") (princ temp_human_score)
										(terpri)
										(princ "Computer score is now ") (princ (+ temp_human_score computer_score))
										(terpri)
										(princ "Human score will remain ") (princ human_score)
										(terpri)
										(cond
											((equal (save_before_game) t)
												(save_to_file score numrounds (create_player_hand 8 tiles ()) (+ human_score computer_score) (create_player_hand 8 tiles ()) human_score () (remove_tiles 16 tiles) () 'neither)
												(exit))
											(t
												(play_tournament board (create_player_hand 8 tiles ()) (create_player_hand 8 (remove_tiles 8 tiles) ()) (remove_tiles 16 tiles) numrounds center score human_score (+ temp_human_score computer_score) temp))))
									(t
										(princ "Both players finished with ") 
										(princ temp_computer_score)
										(terpri)
										(princ "Human score will remain ") 
										(princ human_score) 
										(princ " and computer score will remain ") 
										(princ computer_score)
										(terpri)
										(cond
											((equal (save_before_game) t)
												(save_to_file score numrounds (create_player_hand 8 tiles ()) computer_score (create_player_hand 8 (remove_tiles 8 tiles) ()) human_score () (remove_tiles 16 tiles) () 'neither)
												(exit))
											(t
												(play_tournament board (create_player_hand 8 tiles ()) (create_player_hand 8 (remove_tiles 8 tiles) ()) (remove_tiles 16 tiles) numrounds center score human_score computer_score temp))))))))))))
	(print_board board)
	(cond
		((equal play_next 'h)
			(princ human) 
			(terpri)
			(print_hand_index human 1)
			(terpri)
			(let* ( (temp (get_index (m_length human))))
					(cond
						((symbolp temp)
							(cond
								((equal temp 'p) ; pass
									(cond
										((has_legal_move human board comp_passed)
											(princ "You have a legal move you cannot pass")
											(play_round board human computer stock play_next temp comp_passed human_passed numrounds human_score computer_score score))
										(t
											(cond
												((null stock)
													(cond
														((equal comp_passed t)
															(princ "Boneyard is empty, computer has passed round will end")
															(terpri))
														(t
															(princ "Boneyard is empty, computer will play")
															(terpri)))
													(play_round board human computer stock 'c temp comp_passed t numrounds human_score computer_score score))
												(t
													(just_passed board (create_player_hand 1 stock human) (remove_tiles 1 stock) temp comp_passed computer numrounds human_score computer_score score))))))
								((equal temp 'h) ; help
									(help board human computer stock temp comp_passed human_passed numrounds human_score computer_score score))
								((equal temp 's) ; save
									(save_to_file score numrounds computer computer_score human human_score board stock comp_passed 'h)
									(exit))))
						(t
							(cond
								((has_legal_move human board comp_passed)
									(cond
										((can_play_tile (get_tile_at_n temp human) board comp_passed)
											(let* ( (m_new_board (play_it (get_tile_at_n temp human) board)))
												(cond
													((equal m_new_board board)
														(princ "Invalid move")
														(terpri)
														(play_round board human computer stock 'h temp comp_passed human_passed numrounds human_score computer_score score))
													(t
														(play_round m_new_board (remove_tile_at_n temp human) computer stock 'c temp comp_passed nil numrounds human_score computer_score score)))))
										(t
											(play_round board human computer stock 'h temp comp_passed human_passed numrounds human_score computer_score score))))
								(t
									(princ "That is not a legal move, press 'h' for help")
									(terpri)
									(play_round board human computer stock play_next temp comp_passed human_passed numrounds human_score computer_score score)))))))
		((equal play_next 'c)
			(princ "Your move is over the computer will play next. Before they make a move would you like to save (y/n): ")
			(cond
				((equal (valid_y_or_n) 'y)
					(save_to_file score numrounds computer computer_score human human_score board stock human_passed 'c)
					(exit))
				(t
					(computer_plays_move computer computer board stock human_passed human temp numrounds human_score computer_score score))))))

#||
Function Name: valid_y_or_n
Purpose: To validate a yes or no choice
Parameters:
	none
Return value
	'y or 'n
Algorithm
	1. Read user choice, if it is 'y or 'n return choice
	2. Otherwise recursively call valid_y_or_n
||#
(defun valid_y_or_n ()
	(let* ((choice (read)))
		(cond
			((equal choice 'y)
				choice)
			((equal choice 'n)
				choice)
			(t
				(princ "Invalid entry, would you like to save (y/n): ")
				(valid_y_or_n)))))

#||
Function Name: play_tournament
Purpose: to play a tournament
Parameters:
	board --> list of dotted pairs
	human --> list of dotted pairs
	computer --> list of dotted pairs
	stock --> list of dotted pairs
	numrounds --> atomic value
	center --> dotted pair
	score --> atomic value
	human_score --> atomic value
	computer_score --> atomic value
	temp --> temporary value
Return value
	call to play round
Algorithm
	1. If either player score is greater than tournament score, print out that a player won and exit
	2. Otherwise it needs to start a new round, so it will recursively fill hands until one player has the engine
	3. Once a player has the engine it will tell the user, remove the tile from the player hand and call play_round
||#
(defun play_tournament (board human computer stock numrounds center score human_score computer_score temp)
	(cond
		((>= human_score score)
			(princ "Human player wins with ") (princ human_score) (princ " points, the tournament score was ") (princ score) (princ " and the computer only reached ") (princ computer_score)
			(exit))
		((>= computer_score score)
			(princ "Computer player wins with ") (princ computer_score) (princ " points, the tournament score was ") (princ score) (princ " and the human only reached ") (princ human_score)
			(exit))
		(t
			(cond
				((has_tile center human)
					(terpri)
					(princ "Human has the engine, he will place it and the computer will move next, press enter to continue...")
					(read-line)
					(let* 	( (human (remove_tile_from_hand center human)))
							(play_round board human computer stock 'c temp () () numrounds human_score computer_score score))) 
				((has_tile center computer)
					(terpri)
					(princ "Computer has the engine, he will place it and the human will move next, press enter to continue...")
					(read-line)
					(let* ( ( computer (remove_tile_from_hand center computer)))
							(play_round board human computer stock 'h temp () () numrounds human_score computer_score score))) 
				(t
					(terpri)
					(princ "Neither player has the engine tile, therefore both players shall draw, press enter to continue...")
					(read-line)
					(let* (		(human (create_player_hand 1 stock human))
								(stock (remove_tiles 1 stock))
								(computer (create_player_hand 1 stock computer))
								(stock (remove_tiles 1 stock))) ;; end of variable decs
						(play_tournament board human computer stock numrounds center score human_score computer_score temp)))))))









;;; THIS IS WHERE THE SERIALIZATION FUNCTIONS BEGIN




#||
Function Name: print_tile_to_file
Purpose: to print a dotted pair to the file in the correct format
Parameters:
	my_file --> open filestream
	tile --> dotted pair
Return value
	none, just prints to file
Algorithm
	1. print the tile to my_file in the form "( l r ) "
||#
(defun print_tile_to_file (my_file tile)
	(princ "( " my_file)
	(princ (first tile) my_file)
	(princ " " my_file)
	(princ (rest tile) my_file)
	(princ " ) " my_file))

#||
Function Name: print_hand_to_file
Purpose: to print a list of dotted pairs to the file
Parameters:
	my_file --> open filestream
	hand --> list of dotted pairs
Return value
	Printed hand in file
Algorithm
	1. If hand is empty then its print ")" to myfile
	2. Otherwise call print_tile_to_file on the first element of hand
	3. Then call print_hand_to_file with the rest of hand
||#
(defun print_hand_to_file (my_file hand)
	(cond
		((null hand)
			(princ ")" my_file))
		(t
			(print_tile_to_file my_file (first hand))
			(print_hand_to_file my_file (rest hand)))))

#||
Function Name: print_board_to_file
Purpose: to print the list of dotted pairs to the file
Parameters:
	my_file --> open filestream
Return value
	empty list
Algorithm
	1. If board is empty return ()
	2. Otherwise call print_tile_to_file with the first element in board
	3. Then call print_board_to_file with the rest of board
||#
(defun print_board_to_file (my_file board)
	(cond
		((null board)
			())
		(t
			(print_tile_to_file my_file (first board))
			(print_board_to_file my_file (rest board)))))

#||
Function Name: print_previous_player_passed_to_file
Purpose: to print whether the t if the previous player passed and "()" if they haven't
Parameters:
	my_file --> open filestream
	prev_passed --> atomic value whether the previous player has passed
Return value
	prints to file
Algorithm
	1. IF prev_passed is true print t to the file
	2. Otherwise print "()"
||#
(defun print_previous_player_passed_to_file (my_file prev_passed)
	(cond
		((equal prev_passed t)
			(princ prev_passed my_file))
		(t
			(princ "()" my_file))))

#||
Function Name: save_to_file
Purpose: To serialize game state
Parameters:
	the gamestate:
		- atom: score - tournament score
		- atom: roundnum - roundnumber
		- list: computer - computer hand
		- atom: computer_score - computer's points
		- list: human - human hand
		- atom: human_score - human's score
		- list: board - board
		- list: stock - the deck
		- atom: prev_passed - true or nil on whether previous player passed
		- atom: next_player - 'h or 'c or anythingg else showing who plays next
Return value
	saved game state to file
Algorithm
	1. Read in filename from user and open the file for output
	2. Line by line print the data to the file using print_hand_to_file and print_board_to_file and print_previous_player_passed_to_file
	3. Once printed close the file
||#
(defun save_to_file (score roundnum computer computer_score human human_score board stock prev_passed next_player)
	(princ "Enter the filename to save to: ")
	(let* ( (filename (read-line))
			(my_file (open filename	:direction :output :if-exists :supersede)))
		(princ "(" my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ score my_file)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ roundnum my_file)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ "( " my_file)
		(print_hand_to_file my_file computer)
		(terpri my_file)
		(princ #\tab my_file)
		(princ computer_score my_file)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ "( " my_file)
		(print_hand_to_file my_file human)
		(terpri my_file)
		(princ #\tab my_file)
		(princ human_score my_file)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ "( " my_file)
		(princ "L " my_file)
		(print_board_to_file my_file board)
		(princ " R" my_file)
		(princ " )" my_file)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(princ "( " my_file)
		(print_hand_to_file my_file stock)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(print_previous_player_passed_to_file my_file prev_passed)
		(terpri my_file)
		(terpri my_file)
		(princ #\tab my_file)
		(cond
			((equal next_player 'c)
				(princ "COMPUTER" my_file))
			((equal next_player 'h)
				(princ "HUMAN" my_file)))
		(terpri my_file)
		(terpri my_file)
		(princ ")" my_file)
		(close my_file))
		(exit))

#||
Function Name: create_board_from_stream
Purpose: to return a list of dotted pairs from a stream
Parameters:
	board --> list of dotted pairs
	stream --> string converted into a list
Return value
	list of dotted pairs
Algorithm
	1. If stream is empty return board
	2. If the first element in stream is not a number call create_board_from_stream with the rest of stream
	3. Otherwise add the first two elements in stream into dotted pairs and onto the end of board and call create_board_from_stream with that
||#
(defun create_board_from_stream (board stream)
	(cond
		((null stream)
			board)
		((symbolp (first stream))
			(create_board_from_stream board (rest stream)))
		((integerp (first (first stream)))
			(cond
				((null board)
					(create_board_from_stream (list (cons (first (first stream)) (first (rest (first stream))))) (rest stream)))
				(t
					(create_board_from_stream (append board (list (cons (first (first stream)) (first (rest (first stream)))))) (rest stream)))))
		(t
			(create_board_from_stream board (rest stream)))))

#||
Function Name: load_board_from_file
Purpose: to call create_board_from_stream with a stream and not a string
Parameters:
	temp --> temp variable 
	line --> string value, line in the file
Return value
	call to create_board_from_stream and return from that would be dotted pairs
Algorithm
	1. Call create_board_from_stream with () and read-from-string with line
||#
(defun load_board_from_file (temp line)
	(create_board_from_stream () (read-from-string line)))

#||
Function Name: load_tournament_from_file
Purpose: to load the game state from the file
Parameters:
	none
Return value
	call to play tournament or play round
Algorithm
	1. Have user enter name of file and open the file for input
	2. Create local variables for the lines in the file and parse them correctly using previous functions
	3. If board is empty then call play_tournament to create a new game out of the state
	4. Otherwise determine which player is next to play and call play_round
||#
(defun load_tournament_from_file ()
	(princ "Enter the name of the load file: ")
	(let* ( (filename (read-line)))
			(cond
				((equal (probe-file filename) nil)
					(princ "Invalid filename")
					(terpri)
					(load_tournament_from_file))
				(t
					(let* (	(my_file (open filename :if-does-not-exist nil :direction :input))
							(line1 (read-line my_file nil))
							(tourn_score (read-from-string (read-line my_file nil)))
							(line3 (read-line my_file nil))
							(roundno (read-from-string (read-line my_file nil)))
							(line5 (read-line my_file nil))
							(comp_hand (load_board_from_file () (read-line my_file nil)))
							(comp_score (read-from-string (read-line my_file nil)))
							(line8 (read-line my_file nil))
							(human_hand (load_board_from_file () (read-line my_file nil)))
							(human_score (read-from-string (read-line my_file nil)))
							(line11 (read-line my_file nil))
							(board (load_board_from_file () (read-line my_file nil)))
							(line13 (read-line my_file nil))
							(stock (load_board_from_file () (read-line my_file nil)))
							(line15 (read-line my_file nil))
							(last_passed (read-from-string (read-line my_file nil)))
							(line17 (read-line my_file nil))
							(next_player (read-line my_file nil))
							(close my_file))
						(cond
							((null board)
								(play_tournament (list (get_engine roundno)) human_hand comp_hand stock roundno (get_engine roundno) tourn_score human_score comp_score ()))
							(t
								(cond
									((equal (read-from-string next_player) 'HUMAN)
										(play_round board human_hand comp_hand stock 'h () last_passed last_passed roundno human_score comp_score tourn_score))
									((equal (read-from-string next_player) 'COMPUTER)
										(play_round board human_hand comp_hand stock 'c () last_passed last_passed roundno human_score comp_score tourn_score))))))))))			

(defun save_before_game ()
	(princ "Would you like to save before we begin(y/n): ")
	(let* 	( (choice (read)))
		(cond
			((equal choice 'y)
				t)
			((equal choice 'n)
				nil)
			(t
				(princ "Invalid entry")
				(terpri)
				(save_before_game)))))







;;; THIS IS WHERE THE STARTUP FUNCTIONS ARE 




#||
Function Name: create_new_game
Purpose: to create a game state and call play_tournament
Parameters:
	none
Return value
	call to play tournament
Algorithm
	1. Create the tiles and the hands set everything correctly and input tournament score
	2. Call play_tournament with the gamestate
||#
(defun create_new_game ()
	(let* (  (tiles (create_tiles 0 0 ()))
			 (tiles (shuffle_tiles tiles ()))
		  	 (human (create_player_hand 8 tiles ()))
		  	 (human_score 0)
		  	 (tiles (remove_tiles 8 tiles))
		  	 (computer(create_player_hand 8 tiles ()))
		  	 (computer_score 0)
		  	 (tiles (remove_tiles 8 tiles))
		  	 (stock tiles)
		  	 (tiles (remove_tiles (m_length stock) tiles))
		  	 (board ())
		  	 (temp ())
		  	 (numrounds 1)
		  	 (center (get_engine numrounds))
		  	 (board (add_center center board))
		  	 (score (get_score))
		  	 (save (save_before_game)))
		(cond
			((equal save t)
				(save_to_file score numrounds computer computer_score human human_score () stock nil 'neither)
				(exit))
			(t
				(play_tournament board human computer stock numrounds center score human_score computer_score temp)))))

#||
Function Name: start
Purpose: to start the game
Parameters:
	none
Return value
	call to load_tournament_from_file or create_new_game
Algorithm
	1. Ask the user if they want to save and load or create a game, if invalid input call start
||#
(defun start ()
	(princ "Would you like to load from saved file (y/n): ")
	(let* 	( (choice (read)))
		(cond
			((equal choice 'y)
				(cond
					((equal (load_tournament_from_file) nil)
						(princ "Invalid filename")
						(terpri)
						(start))))
			((equal choice 'n)
				(create_new_game))
			(t
				(princ "Invalid input")
				(terpri)
				(start)))))

; call to start program below
(start)