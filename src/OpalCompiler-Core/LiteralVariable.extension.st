Extension { #name : 'LiteralVariable' }

{ #category : '*OpalCompiler-Core' }
LiteralVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitLiteralVariableNode: aNode
]
