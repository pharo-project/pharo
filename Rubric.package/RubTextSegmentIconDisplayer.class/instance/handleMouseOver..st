event handling
handleMouseOver: anEvent
	| lineIndex |
	super handleMouseOver: anEvent.
	lineIndex := self lineIndexForPoint: anEvent position.
	self mouseEnter: anEvent in: ( (self paragraphProvider segmentsAtLine: lineIndex) select: [ :s | s icon notNil ] ).
	previouslyIn ifNotNil: [
		previouslyIn showMouseHasLeaved.
		previouslyIn := nil ].

	
