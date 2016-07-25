setting
nodesAndNextFrom: aNodeList
	aNodeList 
		do: [ :node | 
				nodes 
					at: node model
					put: (self nodeClass with: node model) ].
	aNodeList 
		do: [ :node |
			node nextNodes 
				do: [ :each |
						(self findNode: node model) 
							to: (self findNode: each model) ] ]