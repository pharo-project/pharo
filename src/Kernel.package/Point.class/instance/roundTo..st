truncation and round off
roundTo: grid
	"Answer a Point that is the receiver's x and y rounded to grid x and 
	grid y."
	
	| gridPoint |
	gridPoint := grid asPoint.
	^ (x roundTo: gridPoint x) @ (y roundTo: gridPoint y)