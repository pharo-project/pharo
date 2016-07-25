event handling
mouseDown: anEvent
	| pos action point |
	self takeKeyboardFocus.
	
	point := anEvent position - layoutViewMorph innerBounds origin.
	pos := layoutViewMorph textPositionAt: point. 
	action := pos span attributeAt: TxActionAttribute.
	(action handleEvent: anEvent) ifTrue: [  ^ self ].
	
	anEvent redButtonPressed ifTrue: [
		anEvent shiftPressed
			ifTrue: [ self selectTo: point ]
			ifFalse: [ self moveTo: point ] ].

	anEvent yellowButtonPressed ifTrue: [
		
		self announcer announce: (TxContextMenuAnnouncement new
			event: anEvent;
			editorMorph: self;
			textPosition: pos;
			yourself
		).
	]