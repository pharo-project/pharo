Extension { #name : 'ClassVariable' }

{ #category : '*AST-Core' }
ClassVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitClassVariableNode: aNode
]
