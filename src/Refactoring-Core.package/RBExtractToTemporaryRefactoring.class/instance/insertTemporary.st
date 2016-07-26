transforming
insertTemporary
	| node statementNode nodeReferences children |
	node := self parseTree whichNodeIsContainedBy: sourceInterval.
	(node notNil and: [ node isValue ])
		ifFalse: [ self refactoringFailure: 'Cannot assign to non-value nodes' ].
	children := self parseTree body allChildren.
	nodeReferences := children select: [ :each | each = node ] thenCollect: [ :each | each ].
	statementNode := node statementNode.
	nodeReferences do: [ :each | each replaceWith: (RBVariableNode named: newVariableName) ].
	statementNode parent
		addNode: (self constructAssignmentFrom: node)
			before:
				(node == statementNode
						ifTrue: [ RBVariableNode named: newVariableName ]
						ifFalse: [ statementNode ]);
		addTemporaryNamed: newVariableName