Extension { #name : 'ThisContextVariable' }

{ #category : '*OpalCompiler-Core' }
ThisContextVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitThisContextNode: aNode
]
