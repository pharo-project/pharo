calculating
totalDistanceTravelled
	| previous sum |
	previous := firstPosition.
	sum := 0.
	move
		do: [ :position | 
			sum := sum + position distanceTo: firstPosition.
			previous := position ].
	^ sum