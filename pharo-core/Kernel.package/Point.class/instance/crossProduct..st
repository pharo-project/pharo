point functions
crossProduct: aPoint 
	"Answer a number that is the cross product of the receiver and the argument, aPoint."

	^ (x * aPoint y) - (y * aPoint x)