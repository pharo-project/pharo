identifier testing
resolvePartialPragmaArgument: aString 
	(#('true' 'false' 'nil' )
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	classOrMetaClass isBehavior
		ifTrue: [classOrMetaClass theNonMetaClass withAllSuperclasses
				do: [:c | 
					(c environment hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					nil]]
		ifFalse: [(environment hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	^ #undefinedIdentifier