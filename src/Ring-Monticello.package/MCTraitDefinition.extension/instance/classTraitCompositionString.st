*Ring-Monticello
classTraitCompositionString
	^self traitComposition 
		ifNil: [ '{}' ]
		ifNotNil: [ :source| | tokens tcs |
			tcs := ''.
			tokens := source parseLiterals.
			tokens do:[ :each| 
				each first isUppercase 
					ifTrue: [ tcs := tcs, each, ' classTrait + ' ] ].
			tcs isEmpty 
				ifTrue: [ '{}' ]
				ifFalse:[ tcs copyFrom: 1 to: tcs size - 3 ] ]