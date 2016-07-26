testing
arePackageNodes: aCollectionOfItem
	^ aCollectionOfItem allSatisfy: [ :node |  node isPackageNode ].