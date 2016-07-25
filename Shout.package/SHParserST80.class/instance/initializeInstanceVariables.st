parse support
initializeInstanceVariables
	instanceVariables := classOrMetaClass notNil
				ifTrue: [classOrMetaClass allInstVarNames asArray]
				ifFalse: [Set new]