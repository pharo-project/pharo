private
removeEdgesOf: aNode
	aNode nextNodes 
		do: [ :node |
				node previousNodes remove: aNode.
				node previousNodes 
					ifEmpty: [ verticesWithNoIncomingEdge add: node ] ].
	aNode nextNodes: #().