adding nodes
addNodeFirst: aNode
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	aNode parent: self.
	^ aNode