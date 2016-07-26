adding nodes
addNode: aNode before: anotherNode 
	| index |
	aNode isReturn 
		ifTrue: [ self error: 'Cannot add return node' ].
	index := self indexOfNode: anotherNode.
	index = 0 ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode beforeIndex: index;
		yourself.
	aNode parent: self.
	^ aNode