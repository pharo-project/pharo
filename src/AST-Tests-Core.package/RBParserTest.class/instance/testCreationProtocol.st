tests
testCreationProtocol
	| messageNode |
	self compare: (RBMessageNode 
				receiver: (RBVariableNode named: 'self')
				selector: #+
				arguments: (Array with: (RBLiteralNode value: 0)))
		to: (self parserClass parseExpression: 'self + 0').
	messageNode := RBMessageNode receiver: (RBVariableNode named: 'self') selector: #foo.
	self compare: (RBMethodNode selector: #bar
				body: (RBSequenceNode statements: (OrderedCollection 
								with: (RBCascadeNode messages: (OrderedCollection with: messageNode with: messageNode)))))
		to: (self parserClass parseMethod: 'bar self foo; foo')