visitor
visitSessionEnd: anEvent
	^ (self displayOperation: 'End:'), 
		(self displayString: anEvent imageName)