visiting
visitTempVariableNode: aTempVariableNode
	(aTempVariableNode isArg
	 or: [written notNil
		and: [written includes: aTempVariableNode]]) ifTrue:
		[^self].
	readBeforeWritten ifNil:
		[readBeforeWritten := IdentitySet new].
	readBeforeWritten add: aTempVariableNode