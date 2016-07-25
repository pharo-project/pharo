event handling
keystrokeFromTextMorph: anEvent
	self eventHandler ifNotNil:
		[^ self eventHandler keyStroke: anEvent fromMorph: self].
	^ false