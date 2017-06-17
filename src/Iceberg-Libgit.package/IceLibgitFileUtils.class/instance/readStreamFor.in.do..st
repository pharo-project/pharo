utilities
readStreamFor: fileName in: tree do: aBlock
	(tree entryByPath: fileName) readStreamDo: aBlock