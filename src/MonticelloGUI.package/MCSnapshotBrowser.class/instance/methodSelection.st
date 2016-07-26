selecting
methodSelection
	^ methodSelection
			ifNil: [0] 
			ifNotNil: [self visibleMethods indexOf: methodSelection]