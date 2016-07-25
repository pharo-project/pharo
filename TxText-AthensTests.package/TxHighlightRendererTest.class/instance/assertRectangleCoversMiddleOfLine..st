asserting
assertRectangleCoversMiddleOfLine: aNumber
	"For a single line selection, simply from the start to end of the selection should be highlighted"

	| expectedStart expectedEnd |
	expectedStart := view layout positionAt: interval start ifOutside: [ self fail ].
	self assert: drawnShapes first topLeft equals: expectedStart topPoint.
	
	expectedEnd := view layout positionAt: interval end ifOutside: [ self fail ].
	self assert: drawnShapes first bottomRight equals: expectedEnd bottomPoint.