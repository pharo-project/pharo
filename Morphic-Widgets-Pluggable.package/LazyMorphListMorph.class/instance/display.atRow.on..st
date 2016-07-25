as yet unclassified
display: item  atRow: row on: aCanvas
	"Display the given item at the given row on the given canvas."
	
	|c frame|
	row = self mouseDownRow ifFalse: [^self].
	frame := self selectionFrameForRow: row.
	c := self colorForRow: row.
	aCanvas 
		frameRectangle: frame
		width: 1
		colors: {c. Color transparent}
		 dashes: #(1 1)