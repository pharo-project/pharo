visiting
visitVariableNode: aVariableNode
	aVariableNode removeProperty: #binding ifAbsent: [].
	aVariableNode primitiveChangeClassTo: RBVariableNode new.