navigation
pageOfNodeIndex: anIndex
	^ (anIndex > 0 and: [anIndex <= self nodeList size])
		ifTrue: [((anIndex - 1) // pageSize) + 1]
		ifFalse: [nil]
			