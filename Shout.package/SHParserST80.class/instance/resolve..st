identifier testing
resolve: aString 
	(#('self' 'super' 'true' 'false' 'nil' 'thisContext' ) includes: aString)
		ifTrue: [^ aString asSymbol].
	(self isBlockTempName: aString)
		ifTrue: [^ #blockTempVar].
	(self isBlockArgName: aString)
		ifTrue: [^ #blockArg].
	(self isMethodTempName: aString)
		ifTrue: [^ #tempVar].
	(self isMethodArgName: aString)
		ifTrue: [^ #methodArg].
	(instanceVariables includes: aString)
		ifTrue: [^ #instVar].
	workspace
		ifNotNil: [(workspace hasBindingOf: aString)
				ifTrue: [^ #workspaceVar]].
	Symbol
		hasInterned: aString
		ifTrue: [:sym | classOrMetaClass isBehavior
				ifTrue: [classOrMetaClass theNonMetaClass withAllSuperclasses
						do: [:c | 
							(c classPool bindingOf: sym)
								ifNotNil: [^ #classVar].
							c sharedPools
								do: [:p | (p bindingOf: sym)
										ifNotNil: [^ #poolConstant]].
							(c environment bindingOf: sym)
								ifNotNil: [^ #globalVar]]]
				ifFalse: [(environment bindingOf: sym)
						ifNotNil: [^ #globalVar]]].
	^ self resolvePartial: aString