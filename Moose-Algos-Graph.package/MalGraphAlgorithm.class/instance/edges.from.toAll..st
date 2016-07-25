building - graph
edges: aCollection from: source toAll: targets
	aCollection 
		do: [ :eModel |
			(targets value: eModel) 
			do: [:target| 
					self 
						addEdge: {source value: eModel . target}
						from: #first
						to: #second ] ]