helpers
drawStuffOn: aForm
	"Draw stuff on aForm. Avoid any symmetry."

	| canvas |
	canvas := FormCanvas on: aForm.
	canvas
		frameAndFillRectangle: (1 @ 1 corner: aForm extent - 15)
		fillColor: Color red
		borderWidth: 3
		borderColor: Color green.
	canvas
		fillOval: (aForm boundingBox topRight - (15 @ -5) extent: 20 @ 20)
		color: Color blue
		borderWidth: 1
		borderColor: Color white.
	^ aForm	"(PNGReadWriterTest new drawStuffOn: (Form extent: 32@32 depth: 16)) display"