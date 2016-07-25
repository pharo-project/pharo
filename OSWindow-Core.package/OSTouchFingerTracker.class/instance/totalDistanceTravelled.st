calculating
totalDistanceTravelled
	| previous sum |
	previous := firstPosition.
	sum := 0.
	move
		do: [ :position | 
			sum := sum + position dist: firstPosition.
			previous := position ].
	^ sum