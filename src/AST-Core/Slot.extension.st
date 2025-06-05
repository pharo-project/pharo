Extension { #name : 'Slot' }

{ #category : '*AST-Core' }
Slot >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitInstanceVariableNode: aNode
]
