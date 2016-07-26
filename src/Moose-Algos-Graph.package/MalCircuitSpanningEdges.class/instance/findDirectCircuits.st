running
findDirectCircuits

	"detect direct circuits"
	| returnNode returnEdge |
	self doUnvisitedEdges: [:e|
		[returnNode := e to.
		 returnEdge := returnNode nextEdges
				detect: [:re| re to == e from].
		 "found a direct circuit"
		 e cseVisited: true.
		 returnEdge cseVisited: true.
		 self addCircuit: (OrderedCollection with: e from model with: e to model)
		] on: Error do: [ "did not find a return edge, so there is no direct circuit"]				
	]