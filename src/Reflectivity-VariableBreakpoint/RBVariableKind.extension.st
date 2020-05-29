Extension { #name : #RBVariableKind }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBVariableKind >> variableValueInContext: aContext [
	^ aContext tempNamed: variable name
]
