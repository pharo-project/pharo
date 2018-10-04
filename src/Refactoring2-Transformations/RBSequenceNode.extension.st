Extension { #name : #RBSequenceNode }

{ #category : #'*Refactoring2-Transformations' }
RBSequenceNode >> addNode: aNode after: anotherNode [

	| index |
	index := self indexOfNode: anotherNode.
	index = 0
		ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode afterIndex: index;
		yourself.
	aNode parent: self.
	^ aNode
]

{ #category : #'*Refactoring2-Transformations' }
RBSequenceNode >> addNodeLast: aNode [

	self lastIsReturn
		ifTrue: [ self addNode: aNode before: self statements last ]
		ifFalse: [ self addNode: aNode ]
]

{ #category : #'*Refactoring2-Transformations' }
RBSequenceNode >> hasTemporaryNamed: aString [

	^ temporaries anySatisfy: [ :temp | temp name = aString ]
]
