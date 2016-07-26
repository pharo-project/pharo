building - graph
edges: aCollection from: source to: target
	aCollection 
		do: [ :eModel |	self addEdge: eModel from: source to: target ]