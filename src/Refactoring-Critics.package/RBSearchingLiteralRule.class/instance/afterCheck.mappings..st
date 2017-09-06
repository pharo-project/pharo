hooks
afterCheck: aNode mappings: mappingDict
	^ self
		isSearchingLiteralExpression: aNode
		for: (mappingDict at: '``@object')