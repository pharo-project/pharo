adding nodes
addNode: aNode
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	aNode parent: self.
	^ aNode