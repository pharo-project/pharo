accessing
nodes: aCollection
	aCollection do: [ :model |
		nodes at: model put: (self nodeClass with: model)
	]