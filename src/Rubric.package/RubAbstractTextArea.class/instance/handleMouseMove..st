event handling
handleMouseMove: anEvent
	"Re-implemented to allow for mouse-up move events"

	anEvent wasHandled
		ifTrue: [ ^ self ].	"not interested"
	anEvent hand hasSubmorphs
		ifTrue: [ ^ self ].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(anEvent anyButtonPressed and: [ anEvent hand mouseFocus == self ])
		ifFalse: [ ^ self ].
	super handleMouseMove: anEvent
	