event handling
mouseEnter: anEvent
	self canResizeColumn ifFalse: [^ self].
	(owner notNil and: [owner bounds containsPoint: anEvent position]) 
		ifTrue: [super mouseEnter: anEvent]