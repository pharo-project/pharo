accessing
findNode: aModel ifAbsent: aBlock
	^ self nodes 
		detect: [:node | node model = aModel]
		ifNone: aBlock