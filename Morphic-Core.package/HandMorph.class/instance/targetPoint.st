accessing
targetPoint
	"Return the new position of the target.
	I.E. return the position of the hand less 
	the original distance between hand and target position"

	^ self position - targetOffset
