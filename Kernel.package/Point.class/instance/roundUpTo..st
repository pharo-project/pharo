truncation and round off
roundUpTo: grid
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y by upper value (toward infinity)."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundUpTo: gridPoint x) @ (y roundUpTo: gridPoint y)