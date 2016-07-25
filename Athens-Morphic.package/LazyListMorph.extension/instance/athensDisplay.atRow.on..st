*Athens-Morphic
athensDisplay: item atRow: row on: aCanvas
	"Display the given item at the given row on the given
	canvas."

	| c backgroundColor drawBounds |

	drawBounds := self drawBoundsForRow: row.
	c := self colorForRow: row.
	backgroundColor := self backgroundColorForRow: row.
	item listRenderOn: aCanvas asCanvasWrapper
		atRow: row
		bounds: drawBounds
		color: c
		backgroundColor: backgroundColor 
		from: self.
	row = self mouseDownRow
		ifTrue: [ 
			(aCanvas setStrokePaint: c)
				width: 1;
				dashes: {1} offset: 0.
			aCanvas drawShape: (self selectionFrameForRow: row) ]