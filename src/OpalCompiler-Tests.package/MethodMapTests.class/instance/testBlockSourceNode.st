testing - ast mapping
testBlockSourceNode
	| sourceNode |
	sourceNode := [ 1 + 2 ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ 1 + 2 ]').

