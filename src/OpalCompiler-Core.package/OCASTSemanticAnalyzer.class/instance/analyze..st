api
analyze: aNode
	self visitNode: aNode.
	OCASTClosureAnalyzer new visitNode: aNode