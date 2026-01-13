Extension { #name : 'SuperVariable' }

{ #category : '*OpalCompiler-Core' }
SuperVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitSuperNode: aNode
]
