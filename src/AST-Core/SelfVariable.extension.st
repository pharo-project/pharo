Extension { #name : 'SelfVariable' }

{ #category : '*AST-Core' }
SelfVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [

	^ aProgramNodeVisitor visitSelfNode: aNode
]
