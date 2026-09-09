Extension { #name : 'ClassVariable' }

{ #category : '*OpalCompiler-Core' }
ClassVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitClassVariableNode: aNode
]
