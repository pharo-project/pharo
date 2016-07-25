generate
genForRBVariableNode
	entity isGlobal ifTrue: [^entity binding value asLiteralNode]. 
	^RBVariableNode named: entity name.