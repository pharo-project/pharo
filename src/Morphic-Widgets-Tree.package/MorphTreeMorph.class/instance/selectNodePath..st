selection
selectNodePath: aPath
	aPath 
		ifNil: [self emptySelection] 
		ifNotNil: [self firstChild 
			ifNotNil: [:fc | fc selectNodePath: aPath]]