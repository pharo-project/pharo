testing
isMultiple
	^ allowsMultipleSelection ifNil: [ allowsMultipleSelection := false ]