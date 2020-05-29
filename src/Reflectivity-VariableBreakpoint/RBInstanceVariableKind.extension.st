Extension { #name : #RBInstanceVariableKind }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBInstanceVariableKind >> variableValueInContext: aContext [
	|receiver|
	receiver := aContext receiver.
	^(receiver class slotNamed: variable name) read: receiver
]
