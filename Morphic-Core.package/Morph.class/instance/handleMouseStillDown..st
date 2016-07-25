events-processing
handleMouseStillDown: anEvent
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	(anEvent hand mouseFocus == self) 
		ifFalse:[^self stopSteppingSelector: #handleMouseStillDown:].
	self mouseStillDown: anEvent.
	^ self eventHandler ifNotNil: [:handler | handler mouseStillDown: anEvent fromMorph: self ]
