error handling
errorBlock
	^errorBlock ifNil: [[:message :position | ]] ifNotNil: [errorBlock]