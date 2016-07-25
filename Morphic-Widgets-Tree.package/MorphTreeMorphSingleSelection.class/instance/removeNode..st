accessing
removeNode: aNode
	self selectedNodePath = aNode path
		ifTrue: [self selectedNodePath: nil]