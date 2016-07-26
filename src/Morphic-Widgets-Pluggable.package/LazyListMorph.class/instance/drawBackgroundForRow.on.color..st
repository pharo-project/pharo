drawing
drawBackgroundForRow: row on: aCanvas color: aColor
	| frame  |
	"shade the background darker, if this row is selected"
 	frame := self selectionFrameForRow: row.
	aCanvas
		fillRectangle: frame
		color: aColor