transforming
extractReceiver
	| name |
	(cascadeNode receiver isLiteralNode or: [ cascadeNode receiver isVariable ])
		ifTrue: [ ^ self ].
	name := self 
		safeVariableNameFor: class 
		temporaries: self parseTree allDefinedVariables 
		basedOn: 'receiver'.
	ancestorNode parent
		addTemporaryNamed: name;
		addNode: (RBAssignmentNode 
			variable: (RBVariableNode named: name)
			value: cascadeNode receiver)
		before: ancestorNode.
	cascadeNode messages
		do: [ :each | each receiver: (RBVariableNode named: name) ]
		