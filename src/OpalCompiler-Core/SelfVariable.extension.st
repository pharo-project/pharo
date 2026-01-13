Extension { #name : 'SelfVariable' }

{ #category : '*OpalCompiler-Core' }
SelfVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [

	^ aProgramNodeVisitor visitSelfNode: aNode
]
