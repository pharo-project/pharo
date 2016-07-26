truncation and round off
roundDownTo: grid
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y by lower value (toward negative infinity)."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundDownTo: gridPoint x) @ (y roundDownTo: gridPoint y)