initialization
populateReferencedNodesWith: nodes

	self referencedGlobalNames do: [ :referencedGlobalName |
		nodes
			detect: [:each | each globalName = referencedGlobalName ]
			ifFound: [ :referencedNode | referencedNodes add: referencedNode ] ] 
