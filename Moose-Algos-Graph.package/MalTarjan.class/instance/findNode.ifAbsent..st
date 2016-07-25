accessing
findNode: aModel ifAbsent: aBlock
	^ nodes 
		at: aModel
		ifAbsent: aBlock