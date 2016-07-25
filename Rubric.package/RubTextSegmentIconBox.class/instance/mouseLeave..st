event handling
mouseLeave: anEvent
	self color: self defaultBackgroundColor.
	previouslyIn
		ifNotNil: [ 
			previouslyIn showMouseHasLeaved.
			previouslyIn := nil ]