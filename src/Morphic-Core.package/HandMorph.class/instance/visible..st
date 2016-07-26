drawing
visible: aBoolean
	self needsToBeDrawn ifFalse: [ ^self ].
	super visible: aBoolean