event handling
mouseEnter: anEvent in: aSegment
	previouslyIn = aSegment
		ifTrue: [ ^ self ].
	previouslyIn := aSegment.
	aSegment showMouseHasEntered 