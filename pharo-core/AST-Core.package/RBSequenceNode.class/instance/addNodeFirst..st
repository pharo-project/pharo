adding nodes
addNodeFirst: aNode
	aNode parent: self.
	statements := statements asOrderedCollection
		addFirst: aNode;
		yourself.
	^ aNode