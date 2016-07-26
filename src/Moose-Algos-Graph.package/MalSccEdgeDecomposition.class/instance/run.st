running
run
	bfsp 
		privateNodes: self nodes;
		privateEdges: self edges.
	self edges do: [:e||c|
		c := self 
			bfsPathFrom: e to 
			to: e from.
		self addCircuit: (self normalize: c) ]