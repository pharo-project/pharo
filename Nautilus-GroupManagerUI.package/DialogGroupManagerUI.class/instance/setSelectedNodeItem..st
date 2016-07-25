tree
setSelectedNodeItem: anItem
	| node |
	node := groupTree listManager nodeMorphsWithAllNodeItems: {anItem}.
	groupTree listManager selectedItems: node.
	^ node