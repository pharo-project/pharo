testing
isDirty
	
	^ self workingCopy
		ifNil: [ false ]
		ifNotNil: [ :wc | wc modified ]