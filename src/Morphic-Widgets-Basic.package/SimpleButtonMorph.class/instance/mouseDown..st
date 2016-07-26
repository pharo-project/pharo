event handling
mouseDown: evt

	super mouseDown: evt.
	evt yellowButtonPressed ifTrue: [ ^self ] .
	oldColor := self fillStyle. 
	actWhen == #buttonDown
		ifTrue: [ self doButtonAction]
		ifFalse: [ self updateVisualState: evt ].
	self mouseStillDown: evt.