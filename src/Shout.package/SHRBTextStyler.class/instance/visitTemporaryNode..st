visiting rb nodes
visitTemporaryNode: aTemporaryNode
	| methodOrBlockTempStyle |
	methodOrBlockTempStyle := self methodOrBlockTempStyleFor: aTemporaryNode.
	self addStyle: methodOrBlockTempStyle forNode: aTemporaryNode