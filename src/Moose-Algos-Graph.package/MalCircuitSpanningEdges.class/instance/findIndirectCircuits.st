running
findIndirectCircuits
	
	"for each unvisited edge, pop and detect circuit with Dijsktra, backtrack"
	dijkstra privateNodes: self nodes; privateEdges: self edges.
	self doUnvisitedEdges: [:e|
		dijkstra reset.
		dijkstra start: e to; end: e from; run. "find circuit leading back from e target to e source"
		(dijkstra pathWeight ~= Float infinity) ifTrue: [
			self addCircuit: (dijkstra backtrack: dijkstra end edgeDo: [:x| x cseVisited: true]).
			"we have the path from e target to e source, and all edges 
			 marked as visited between.
			 We have to mark the seeding edge"
			e cseVisited: true ]
		]