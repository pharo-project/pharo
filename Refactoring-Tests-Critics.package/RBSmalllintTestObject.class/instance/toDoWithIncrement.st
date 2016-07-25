methods
toDoWithIncrement
	| counter |
	counter := 0.
	1 to: 10
		by: 2
		do: [:i | counter := counter + 2].
	^counter