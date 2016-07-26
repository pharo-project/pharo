events-processing
handleMouseMove: anEvent
	"System level event handling."
	anEvent wasHandled ifTrue:[^self]. 
	
	(self handlesMouseMove: anEvent) ifFalse: [ ^self ].

	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: self mouseStillDownStepRate ].
	^ self eventHandler ifNotNil: [:handler | handler mouseMove: anEvent fromMorph: self ]
