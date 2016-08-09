adding nodes
addFaultyNode: aNode
"just like addNode, but ignore if we already include a return node, as we are faulty"
	aNode parent: self.
	statements := statements asOrderedCollection
		add: aNode;
		yourself.
	^ aNode