instructions
jumpForward: distance

	distance = 0 ifTrue: [^ self].  
	encoder genJump: distance.
