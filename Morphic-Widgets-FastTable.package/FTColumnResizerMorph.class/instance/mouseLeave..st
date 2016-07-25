event handling
mouseLeave: anEvent
	self canResizeColumn ifFalse: [^ self].
	super mouseLeave: anEvent