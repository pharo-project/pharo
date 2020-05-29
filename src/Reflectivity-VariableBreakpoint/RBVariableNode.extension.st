Extension { #name : #RBVariableNode }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBVariableNode >> hasBreakpoint [
	^ (self afterLinks anySatisfy: [ :link | link metaObject = Break ])
		or: [ super hasBreakpoint ]
]

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBVariableNode >> variableValueInContext: aContext [
	^ kind variableValueInContext: aContext
]
