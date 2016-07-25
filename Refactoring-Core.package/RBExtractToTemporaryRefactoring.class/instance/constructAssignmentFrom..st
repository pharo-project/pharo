transforming
constructAssignmentFrom: aNode 
	| valueNode |
	valueNode := RBVariableNode named: newVariableName.
	^RBAssignmentNode variable: valueNode value: aNode