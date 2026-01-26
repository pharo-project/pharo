Extension { #name : 'OCSequenceNode' }

{ #category : '*Refactoring-Transformations' }
OCSequenceNode >> addNodeLast: aNode [

	self lastIsReturn
		ifTrue: [ self addNode: aNode before: self statements last ]
		ifFalse: [ self addNode: aNode ]
]
