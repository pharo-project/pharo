visitor
visitSessionStart: anEvent

	^ (self displayOperation: 'Start:'), 
		(self displayString: anEvent imageName)