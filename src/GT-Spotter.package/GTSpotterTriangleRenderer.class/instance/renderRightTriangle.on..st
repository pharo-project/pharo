rendering
renderRightTriangle: aBrick on: aCanvas
	|a b c width height gap|
	
	gap := aBrick themer spotterThemer previewArrowGapHack.
	aCanvas fillRectangle: (aBrick brickBounds withWidth: ((gap - self border)max:0)) color: aBrick color.
	
	width := aBrick brickBounds width - gap.
	height := aBrick brickBounds height.
	
	a := aBrick brickBounds topLeft + (gap @ ((height / 2 - width) max: 0)) - (0@1).
	b := aBrick brickBounds topLeft + (gap @ ((height / 2 + width) min: height)) - (0@self border negated) - (0@self border).
	c := aBrick brickBounds topRight + (0 @ (height / 2)).
	
	self border > 0
		ifTrue: [ 
			self renderTriangle: aBrick on: aCanvas with: { a asIntegerPoint . b asIntegerPoint . c asIntegerPoint } color: self borderColor.
			self renderTriangle: aBrick on: aCanvas with: { a asIntegerPoint - ((self border+1)@0) . b asIntegerPoint - ((self border+1)@0) . c asIntegerPoint - ((self border+1)@0) } ]
		ifFalse: [ 
			self renderTriangle: aBrick on: aCanvas with: { a asIntegerPoint . b asIntegerPoint . c asIntegerPoint }.
		]