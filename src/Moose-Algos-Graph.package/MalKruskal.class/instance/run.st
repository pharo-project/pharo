running
run
	treeEdges := OrderedCollection new.
	self sortedEdges: edges.
	nodes do: #makeSet.
	sortedEdges do: [ :edge|
		(edge from find = edge to find) 
		ifFalse: [ edge from union: edge to.
					treeEdges add: edge ] ].
	^ treeEdges 