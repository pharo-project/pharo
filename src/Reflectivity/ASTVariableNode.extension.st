Extension { #name : 'ASTVariableNode' }

{ #category : '*Reflectivity' }
ASTVariableNode >> variableValueInContext: aContext [
	^ variable readInContext: aContext
]
