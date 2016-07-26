event handling
handleMouseMove: anEvent
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent hand hasSubmorphs) ifTrue:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: 1].
