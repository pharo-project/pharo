Extension { #name : 'GlobalVariable' }

{ #category : '*OpalCompiler-Core' }
GlobalVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitGlobalVariableNode: aNode
]
