transforming
compileDelegatorMethod
	| statementNode delegatorNode tree |
	delegatorNode := RBMessageNode 
				receiver: (RBVariableNode named: variable)
				selector: parseTree selector
				keywordsPositions: parseTree keywordsPositions
				arguments: (parseTree argumentNames collect: 
							[:each | 
							RBVariableNode 
								named: (each = selfVariableName ifTrue: ['self'] ifFalse: [each])]).
	self hasOnlySelfReturns 
		ifFalse: [delegatorNode := RBReturnNode value: delegatorNode].
	statementNode := RBSequenceNode temporaries: #()
				statements: (Array with: delegatorNode).
	(tree := class parseTreeFor: selector) body: statementNode.
	class compileTree: tree