event handling
keyStroke: event
	(super keyStroke: event) ifTrue: [ ^ true ].
	self keystrokeSelector 
		ifNotNil: [ (self keyStrokeAction: event) ifTrue: [ ^ true ] ].
	(self keystrokeActionSelector notNil 
		and: [ event anyModifierKeyPressed ])
		ifTrue: [ (self keyStrokeModifierAction: event) ifTrue: [ ^ true ] ].
	
	^ false