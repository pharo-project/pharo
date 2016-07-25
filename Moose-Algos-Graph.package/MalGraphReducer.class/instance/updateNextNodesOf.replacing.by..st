updating
updateNextNodesOf: mergedNode replacing: aNodeList by: anotherNode
	mergedNode nextNodes
		do: [ :node | 
			node previousNodes
				removeAllFoundIn: aNodeList;
				addIfNotPresent: anotherNode ]