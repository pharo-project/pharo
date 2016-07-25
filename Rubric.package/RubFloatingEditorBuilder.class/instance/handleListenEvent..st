events-processing
handleListenEvent: anEvent
	anEvent isMouse
		ifFalse: [ ^ self ].
	anEvent isMouseDown
		ifFalse: [ ^ self ].
	(editor boundsInWorld containsPoint: anEvent position)
		ifFalse: [ self closeEditor ]