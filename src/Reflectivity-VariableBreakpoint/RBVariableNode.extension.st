Extension { #name : #RBVariableNode }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBVariableNode >> variableValueInContext: aContext [
	^ variable readInContext: aContext
]
