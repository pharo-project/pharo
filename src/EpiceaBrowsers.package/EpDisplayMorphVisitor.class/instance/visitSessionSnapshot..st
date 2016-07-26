visitor
visitSessionSnapshot: anEvent
	^ (self displayOperation: 'Snapshot:'), 
		(self displayString: anEvent imageName)