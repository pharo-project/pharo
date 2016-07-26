format
instanceSpecification
	^ self hasFields
		ifTrue: [ 3 ]
		ifFalse: [ 2 ]