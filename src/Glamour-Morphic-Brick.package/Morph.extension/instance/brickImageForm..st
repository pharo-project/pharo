*Glamour-Morphic-Brick
brickImageForm: aBackground
	| background form brickBorder |
	
	self isBrick ifFalse: [
		^ self imageForm ].

	brickBorder := self border.
	self border: GLMBrickBorder new.
	
	background := self color.
	self color: aBackground.
	form := self imageForm.
	self color: background.
	
	self border: brickBorder.
	
	^ form