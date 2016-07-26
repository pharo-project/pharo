event field
dDist
	| directionDDist |
	directionDDist := (self activeFingers collect: [ :each | each delta ])
		average * direction abs.
	^ directionDDist x + directionDDist y