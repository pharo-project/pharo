drawing
drawOn: aCanvas
	"Draw the grip on the given canvas."

	| dotBounds alphaCanvas windowBorderWidth dotBounds2 |
	
	self shouldDraw ifFalse: [^self].

	windowBorderWidth := SystemWindow borderWidth.
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.	
	"alphaCanvas
		frameRectangle: bounds color: Color blue."

	dotBounds := self bounds.
	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 top: (dotBounds2 bottom - windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.
	
	dotBounds2 := dotBounds left: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 top: (dotBounds2 bottom - windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 left: (dotBounds2 left + 7).
	dotBounds2 := dotBounds2 right: (dotBounds2 right - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor.

	dotBounds2 := dotBounds right: (dotBounds left + windowBorderWidth).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 bottom - windowBorderWidth).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self handleColor.

	dotBounds2 := dotBounds2 top: (dotBounds2 top + 7).
	dotBounds2 := dotBounds2 bottom: (dotBounds2 bottom - 7).
	alphaCanvas
		fillRectangle: dotBounds2
		color: self dotColor