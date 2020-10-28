Extension { #name : #RBVariableNode }

{ #category : #'*Reflectivity' }
RBVariableNode >> variableValueInContext: aContext [
	^ variable readInContext: aContext
]
