asserting
assertRectangle: rectangleNumber coversRightPartOfLine: lineNumber 
	"For the first line of a multi-line highlight, cover from the start of the selection to the right side of the view"

	| line rectangle expectedStart expectedEndPoint lineEndPosition |
	line := view layout lines at: lineNumber.
	rectangle := drawnShapes at: rectangleNumber.
	expectedStart := view layout positionAt: interval start ifOutside: [ self fail ].
	self assert: rectangle topLeft equals: expectedStart topPoint.
	lineEndPosition := view layout positionAt: line endPosition ifOutside: [ self fail ].
	expectedEndPoint := view width @ lineEndPosition bottomPoint y.
	self assert: rectangle bottomRight equals: expectedEndPoint.