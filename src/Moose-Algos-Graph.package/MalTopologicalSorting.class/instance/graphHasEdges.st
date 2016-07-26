private
graphHasEdges
	^ (nodes allSatisfy: [ :node | node nextNodes isEmpty ]) not