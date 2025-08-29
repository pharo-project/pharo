Extension { #name : 'SuperVariable' }

{ #category : '*AST-Core' }
SuperVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitSuperNode: aNode
]
