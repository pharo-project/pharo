private
resolveStyleFor: aVariableNode
	aVariableNode binding isUndeclared ifTrue: [
		| var |
		var := aVariableNode scope findVariable: [ :varS | varS name beginsWith: aVariableNode name ] ifNone: [ nil ].
		var ifNil: [ #default ] ifNotNil: [ #incompleteIdentifier ].
	].
	aVariableNode isArg ifTrue: [ ^#methodArg].
	aVariableNode isTemp ifTrue: [ ^#tempVar].
	aVariableNode isInstance ifTrue: [ ^#instVar]. 

	aVariableNode isUndeclared ifTrue: [ ^#invalid]. 
	aVariableNode isGlobal ifTrue: [ ^  #globalVar ].
	self flag: 'todo: see if its incompleteIdentifier'.
	^#invalid.