private
computeVerticesWithNoIncomingEdge
	verticesWithNoIncomingEdge := nodes select: [ :node | node previousNodes isEmpty ]