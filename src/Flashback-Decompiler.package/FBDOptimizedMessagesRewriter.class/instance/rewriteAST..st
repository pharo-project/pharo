public api
rewriteAST: aMethodNode
	self visitNode: aMethodNode.
	^ aMethodNode