event handling
mouseEnter: anEvent in: aSegmentList
	aSegmentList ifEmpty: [ ^self ].
	previouslyIn = aSegmentList first
		ifTrue: [ ^ self ].
	previouslyIn := aSegmentList first.
	self invokeIconMenuOfSegments: aSegmentList event: anEvent.
