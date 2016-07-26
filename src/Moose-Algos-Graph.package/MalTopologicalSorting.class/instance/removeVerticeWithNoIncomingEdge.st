private
removeVerticeWithNoIncomingEdge
	| vertice |
	
	vertice := verticesWithNoIncomingEdge anyOne.
	verticesWithNoIncomingEdge remove: vertice.
	^ vertice	