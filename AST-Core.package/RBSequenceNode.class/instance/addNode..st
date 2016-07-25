adding nodes
addNode: aNode
	aNode parent: self.
	(statements notEmpty and: [ statements last isReturn ])
		ifTrue: [ self error: 'Cannot add statement after return node' ].
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	^ aNode