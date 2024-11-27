Extension { #name : 'ASTAssignmentNode' }

{ #category : '*Reflectivity' }
ASTAssignmentNode >> variableValueInContext: aContext [
	^self variable variableValueInContext: aContext
]
