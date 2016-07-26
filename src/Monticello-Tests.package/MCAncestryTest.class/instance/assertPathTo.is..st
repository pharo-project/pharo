asserting
assertPathTo: aSymbol is: anArray
	self
		assertNamesOf: (self tree allAncestorsOnPathTo: (self treeFrom: {aSymbol}))
		are: anArray