Extension { #name : #RBVariableNode }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBVariableNode >> variableValueInContext: aContext [
	^ aContext tempNamed: self name
]
