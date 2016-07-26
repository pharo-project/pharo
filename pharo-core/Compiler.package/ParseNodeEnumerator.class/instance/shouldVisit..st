testing
shouldVisit: aNode
	^theSelectBlock isNil or: [theSelectBlock value: aNode]