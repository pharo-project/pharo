Extension { #name : 'LiteralVariable' }

{ #category : '*AST-Core' }
LiteralVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitLiteralVariableNode: aNode
]
