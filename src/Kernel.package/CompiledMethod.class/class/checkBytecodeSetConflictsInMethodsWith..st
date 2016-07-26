class initialization
checkBytecodeSetConflictsInMethodsWith: aBlock
	(self allSubInstances detect: aBlock ifNone: [ ])
		ifNotNil: [ Warning signal: 'There are existing CompiledMethods with a different encoderClass.' ]