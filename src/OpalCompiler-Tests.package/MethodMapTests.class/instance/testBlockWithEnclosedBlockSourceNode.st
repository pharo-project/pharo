testing - ast mapping
testBlockWithEnclosedBlockSourceNode
	| sourceNode |
	sourceNode := [ [ ] ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ [ ] ]').
