building - graph
edges: aCollection from: source to: target weight: weightFunction
	| edge |
	aCollection 
		do: [ :eModel |
			edge := self addEdge: eModel from: source to: target.
			edge ifNotNil: [edge weight: (weightFunction value: eModel)] ]