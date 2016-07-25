api
analyze: aNode
	self visitNode: aNode.
	RFASTClosureAnalyzer new visitNode: aNode