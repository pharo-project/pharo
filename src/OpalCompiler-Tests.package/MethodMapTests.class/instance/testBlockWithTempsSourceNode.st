testing - ast mapping
testBlockWithTempsSourceNode
	| sourceNode |
	sourceNode := [ | t1 t2 | ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ | t1 t2 | ]').
