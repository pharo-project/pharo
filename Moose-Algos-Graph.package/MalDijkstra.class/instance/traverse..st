running
traverse: node

	self flag: #todo. "optimize traversal of nodes"
	node nextEdges do: [:edge |
		edge isVisited ifFalse: [
			edge to pathWeight: (edge to pathWeight min: (node pathWeight + edge weight)).
			edge visited: true.
			self traverse: edge to ]]
