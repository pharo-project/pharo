brick-structure-testing
isLastInOwner

	^ self brickDoubleLink
		ifNotNil: [ :bdl | bdl hasSuccessor not ]
		ifNil: [ false ]