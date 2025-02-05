Extension { #name : 'OCVariableNode' }

{ #category : '*Reflectivity' }
OCVariableNode >> variableValueInContext: aContext [
	^ variable readInContext: aContext
]
