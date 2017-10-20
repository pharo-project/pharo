tests
testPositions
	| blockNode |
	blockNode := self parserClass parseExpression: '[:a :b | ]'.
	self assert: blockNode left = 1.
	self assert: blockNode right = 10.
	self assert: blockNode bar = 8.
	self assert: blockNode sourceInterval = (1 to: 10).
	self assert: blockNode size = 1.	"test dummy collection protocol"
	blockNode printString.	"coverage"
	self deny: (blockNode isLast: (RBVariableNode named: 'b')).
	self compare: blockNode
		to: (RBBlockNode 
				arguments: (OrderedCollection with: (RBVariableNode named: 'a')
						with: (RBVariableNode named: 'b'))
				body: (RBSequenceNode statements: OrderedCollection new)).
				