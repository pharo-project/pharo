identifier testing
resolvePragmaArgument: aString 
	(#('true' 'false' 'nil' ) includes: aString)
		ifTrue: [^ aString asSymbol].
	Symbol
		hasInterned: aString
		ifTrue: [:sym | classOrMetaClass isBehavior
				ifTrue: [classOrMetaClass theNonMetaClass withAllSuperclasses
						do: [:c | (c environment bindingOf: sym)
								ifNotNil: [^ #globalVar]]]
				ifFalse: [(environment bindingOf: sym)
						ifNotNil: [^ #globalVar]]].
	^ self resolvePartialPragmaArgument: aString