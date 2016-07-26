format
instanceSpecification
	^ self hasFields
		ifTrue: [ 1 ]
		ifFalse: [ 0 ]