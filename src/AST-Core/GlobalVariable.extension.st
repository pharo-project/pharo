Extension { #name : 'GlobalVariable' }

{ #category : '*AST-Core' }
GlobalVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitGlobalVariableNode: aNode
]
