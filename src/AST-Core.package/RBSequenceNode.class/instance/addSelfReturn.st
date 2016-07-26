adding nodes
addSelfReturn
	| node |
	self lastIsReturn
		ifTrue: [ ^ self statements last ].
	node := RBReturnNode value: (RBVariableNode named: 'self').
	^ self addNode: node