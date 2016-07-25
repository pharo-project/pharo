testing - ast mapping
testBlockWithArgAndEnclosedBlockSourceNode
	| sourceNode |
	sourceNode := [ :arg |  [ arg ] ] sourceNode.
	self assert: sourceNode equals: (RBParser parseExpression: '[ :arg | [ arg ] ]').
