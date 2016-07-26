accessing
findNode: aModel ifFound: aBlock
	^ self nodes 
		detect: [:node | node model = aModel]
		ifFound: aBlock