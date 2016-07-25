adding nodes
addNode: aNode before: anotherNode
	| index |
	index := self indexOfNode: anotherNode.
	index = 0
		ifTrue: [ ^ self addNode: aNode ].
	statements := statements asOrderedCollection
		add: aNode beforeIndex: index;
		yourself.
	aNode parent: self.
	^ aNode