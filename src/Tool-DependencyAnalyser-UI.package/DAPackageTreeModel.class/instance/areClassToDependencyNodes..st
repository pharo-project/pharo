testing
areClassToDependencyNodes: aCollectionOfItem
	^ aCollectionOfItem allSatisfy: [ :node |  node isClassToDependencyNode ].