testing
isWrite
	^ self parent notNil and: [ self parent isAssignment and: [ self parent variable == self ] ]