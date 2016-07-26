running
run
	self computeVerticesWithNoIncomingEdge.
	[ verticesWithNoIncomingEdge isEmpty ]
		whileFalse: [ | node |
			node := self removeVerticeWithNoIncomingEdge.
			sortedElements addLast: node.
			self removeEdgesOf: node ].
	self graphHasEdges
		ifTrue: [ Error signal: 'Not a DAG (Directed Acyclic Graph)' ].
	^ sortedElements reverse