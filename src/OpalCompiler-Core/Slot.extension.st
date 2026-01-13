Extension { #name : 'Slot' }

{ #category : '*OpalCompiler-Core' }
Slot >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitInstanceVariableNode: aNode
]
