asserting
assertRectangle: rectangleNumber coversFullViewWidthOfLine: lineNumber
	"When a highlighted line is not the first or the last, highlight from view left to view right"

	| line rectangle expectedStart expectedEndPoint lineEndPosition |
	line := view layout lines at: lineNumber.
	rectangle := drawnShapes at: rectangleNumber.
	expectedStart := view layout positionAt: line startPosition ifOutside: [ self fail ].
	self assert: rectangle topLeft equals: expectedStart topPoint.
	lineEndPosition := view layout positionAt: line endPosition ifOutside: [ self fail ].
	expectedEndPoint := view width @ lineEndPosition bottomPoint y.
	self assert: rectangle bottomRight equals: expectedEndPoint.