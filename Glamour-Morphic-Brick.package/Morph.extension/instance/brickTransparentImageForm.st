*Glamour-Morphic-Brick
brickTransparentImageForm
	|background form brickBorder|
	
	self isBrick ifTrue: [ 
		brickBorder := self border.
		self border: GLMBrickBorder new ].
	
	background := self color.
	self color: Color transparent.
	form := self imageForm.
	self color: background.
	
	self isBrick ifTrue: [ 
		self border: brickBorder ].
	^ form