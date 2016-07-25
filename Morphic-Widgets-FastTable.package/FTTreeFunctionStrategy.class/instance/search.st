action
search
	^ self optionalOperations 
		ifNotNil: [ :res | res ]
		ifNil: [ self realSearch]