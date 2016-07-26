events-processing
handleMouseMove: anEvent
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	
	anEvent wasHandled 
		ifTrue:[^self]. 	
	(anEvent isDraggingEvent or:[ anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]]) 
		ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) 
		ifTrue: [
			"Step at the new location"
			self startStepping: #handleMouseStillDown: 
				at: Time millisecondClockValue + self mouseStillDownThreshold
				arguments: {anEvent copy resetHandlerFields}
				stepTime: self mouseStillDownStepRate].
