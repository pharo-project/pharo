private
resolveStyleFor: aVariableNode
	aVariableNode binding ifNil: [^#default].
	aVariableNode isArg ifTrue: [ ^#methodArg].
	aVariableNode isTemp ifTrue: [ ^#tempVar].
	aVariableNode isGlobal ifTrue: [ ^#globalVar].
	aVariableNode isInstance ifTrue: [ ^#instVar]. 
	aVariableNode hasIncompleteIdentifier ifTrue:[ ^#incompleteIdentifier].
	^#invalid.