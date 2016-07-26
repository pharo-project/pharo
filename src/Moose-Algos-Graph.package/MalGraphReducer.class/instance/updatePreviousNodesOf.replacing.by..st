updating
updatePreviousNodesOf: mergedNode replacing: aNodeList by: anotherNode
	mergedNode previousNodes
		do: [ :node | 
			node nextNodes
				removeAllFoundIn: aNodeList;
				addIfNotPresent: anotherNode ]