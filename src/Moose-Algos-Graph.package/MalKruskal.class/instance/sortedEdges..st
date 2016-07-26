running
sortedEdges: aCollectionOfWeightedEdges
	sortedEdges := SortedCollection sortBlock: sortBlock.
	sortedEdges addAll: aCollectionOfWeightedEdges 