row management
display: item atRow: row on: aCanvas
	"Display the given item at the given row on the given
	canvas."

	| itemColor backgroundColor drawBounds |
	drawBounds := self drawBoundsForRow: row.
	itemColor := self colorForRow: row.
	backgroundColor := self backgroundColorForRow: row.
	item
		listRenderOn: aCanvas
		atRow: row
		bounds: drawBounds
		color: itemColor
		backgroundColor: backgroundColor 
		from: self.
		
	row = self mouseDownRow
		ifTrue: [ 
			aCanvas
				frameRectangle: (self selectionFrameForRow: row)
				width: 1
				colors: {itemColor. (Color transparent)}
				dashes: #(1 1) ]