Extension { #name : #RBInstanceVariableNode }

{ #category : #'*Reflectivity-VariableBreakpoint' }
RBInstanceVariableNode >> variableValueInContext: aContext [
	|receiver|
	receiver := aContext receiver.
	^(receiver class slotNamed: self name) read: receiver
]
