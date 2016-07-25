building - graph
addNodesFrom: model childrenBlock: childrenBlock
	"recursively add nodes from the model"
	| parent |
	model ifNil: [ ^ self ].

	self
		findNode: model
		ifFound: [ ^ self "Prevent cycles. Do not add a node already added." ].

	parent := self addNodeFor: model.
	(childrenBlock value: model)
		do: [ :child | 
				self addNodesFrom: child childrenBlock: childrenBlock.
				self addEdge: { parent model . child } from: #first to: #second ]