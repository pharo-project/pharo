Extension { #name : 'Variable' }

{ #category : '*AST-Core' }
Variable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitVariableNode: aNode
]
