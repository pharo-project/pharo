visiting
visitWindowFrameEvent: anEvent
	example ifNotNil: [ example update: Time millisecondClockValue *0.001. ].
	example enqueueRender 