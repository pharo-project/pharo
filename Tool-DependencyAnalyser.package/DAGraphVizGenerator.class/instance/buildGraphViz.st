build items
buildGraphViz
	graphviz := (self class environment at: #GVGraphViz) new.
	graphviz
		beDirected;
		name: self nameOfGraph.
	graphviz add: #node with: {(#shape -> #box)}.
	graphviz add: #graph with: { (#label -> self nameOfGraph). (#fontsize -> 20) }.	
	
	self
		buildNodes;
		buildEdges.