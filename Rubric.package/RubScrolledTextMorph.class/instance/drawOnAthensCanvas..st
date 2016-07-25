drawing
drawOnAthensCanvas: anAthensCanvas
	"Indicate unaccepted edits, conflicts etc."

	super drawOnAthensCanvas: anAthensCanvas. 
	self drawHighlightsOn: anAthensCanvas asCanvasWrapper