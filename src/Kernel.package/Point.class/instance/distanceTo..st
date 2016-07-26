point functions
distanceTo: aPoint 
	"Answer the distance between aPoint and the receiver."
	| dx dy |
	dx := aPoint x - x.
	dy := aPoint y - y.
	^ (dx * dx + (dy * dy)) sqrt