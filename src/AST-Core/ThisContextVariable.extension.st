Extension { #name : 'ThisContextVariable' }

{ #category : '*AST-Core' }
ThisContextVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitThisContextNode: aNode
]
