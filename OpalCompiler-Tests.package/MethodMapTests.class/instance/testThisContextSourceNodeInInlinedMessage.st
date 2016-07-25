testing - ast mapping
testThisContextSourceNodeInInlinedMessage
	| inlinedBlockSourceNode |
	inlinedBlockSourceNode := self inlinedBlockSourceNode.
	self assert: (inlinedBlockSourceNode isKindOf: RBBlockNode).
	self assert: inlinedBlockSourceNode equals: (RBParser parseExpression: '[ :i | ^ thisContext sourceNode ]')