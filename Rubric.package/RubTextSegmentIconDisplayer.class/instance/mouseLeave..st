event handling
mouseLeave: anEvent
	previouslyIn
		ifNotNil: [ 
			previouslyIn showMouseHasLeaved.
			previouslyIn := nil ]