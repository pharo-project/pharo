helpers
drawTransparentStuffOn: aForm
	"Draw stuff on aForm. Avoid any symmetry."

	| canvas |
	canvas := FormCanvas on: aForm.
	canvas
		frameAndFillRectangle: (1 @ 1 corner: aForm extent - 15)
		fillColor: (Color red alpha: 0.25)
		borderWidth: 3
		borderColor: (Color green alpha: 0.5).
	canvas
		fillOval: (aForm boundingBox topRight - (15 @ -5) extent: 20 @ 20)
		color: (Color white alpha: 0.75)
		borderWidth: 1
		borderColor: Color blue.
	^ aForm	"(PNGReadWriterTest new drawStuffOn: (Form extent: 32@32 depth: 16)) display"