examples - branches
exampleNestedIfNilIfNotNil
	^ true
		ifNotNil: [ 1 ]
		ifNil: [ false 
			ifNotNil: [ 2 ] 
			ifNil: [ ^ true ] ]