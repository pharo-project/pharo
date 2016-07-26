selecting
selection
	^ selection 
		ifNil: [0]
		ifNotNil: [self items indexOf: selection]