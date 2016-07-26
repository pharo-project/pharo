generate
genForRBVariableNode
	
	entity isTemp ifTrue: [ ^self error: 'Temps can not be reified yet' ]. 
	^entity binding variable asLiteralNode