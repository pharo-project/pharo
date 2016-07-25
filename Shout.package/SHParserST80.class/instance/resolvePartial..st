identifier testing
resolvePartial: aString 
	(#('self' 'super' 'true' 'false' 'nil' 'thisContext' )
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteBlockTempName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteBlockArgName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteMethodTempName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteMethodArgName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(instanceVariables
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	workspace
		ifNotNil: [(workspace hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	classOrMetaClass isBehavior
		ifTrue: [classOrMetaClass theNonMetaClass withAllSuperclasses
				do: [:c | 
					(c classPool hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					c sharedPools
						do: [:p | 
							(p hasBindingThatBeginsWith: aString)
								ifTrue: [^ #incompleteIdentifier].
							nil].
					(c environment hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					nil]]
		ifFalse: [(environment hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	^ #undefinedIdentifier