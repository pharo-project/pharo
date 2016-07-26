asserting
assertRectangle: rectangleNumber coversLeftPartOfLine: lineNumber
	"For the last line of a multi-line highlight, cover from the start of the line to the end of the selection"

	| line rectangle expectedStart expectedEnd |
	line := view layout lines at: lineNumber.
	rectangle := drawnShapes at: rectangleNumber.
	expectedStart := view layout positionAt: line startPosition ifOutside: [ self fail ].
	self assert: rectangle topLeft equals: expectedStart topPoint.
	expectedEnd := view layout positionAt: interval end ifOutside: [ self fail ].
	self assert: rectangle bottomRight equals: expectedEnd bottomPoint.