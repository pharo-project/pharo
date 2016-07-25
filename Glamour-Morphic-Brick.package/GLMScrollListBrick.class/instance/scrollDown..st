scrolling
scrollDown: anInteger
	
	self scrollBy: (
		self isLastAdded
			ifTrue: [ anInteger min: (self band brickBounds bottom - self height) ]
			ifFalse: [ anInteger ]) negated