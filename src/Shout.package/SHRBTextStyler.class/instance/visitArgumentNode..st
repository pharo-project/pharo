visiting rb nodes
visitArgumentNode: anArgumentNode
	| blockOrMethodArgStyle |
	blockOrMethodArgStyle := self methodOrBlockArgStyleFor: anArgumentNode.
	self addStyle: blockOrMethodArgStyle forNode: anArgumentNode