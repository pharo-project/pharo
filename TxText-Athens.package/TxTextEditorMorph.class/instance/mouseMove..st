event handling
mouseMove: anEvent

	anEvent hand showTemporaryCursor: (self cursorForPosition: anEvent position).

	anEvent redButtonPressed "left button" ifTrue: [ 
		(anEvent position y between: self top and: self bottom) ifFalse:[
			self startStepping: #autoScrollView:
			at: Time millisecondClockValue
			arguments: (Array with: anEvent)
			stepTime: 100. "fast enough" ]
		ifTrue: [ 		
			self stopSteppingSelector: #autoScrollView:.
			self selectTo: anEvent position - layoutViewMorph innerBounds origin.
		]
	 ]