comparing
sameLiteralsAs: method
	"Compare my literals to those of method. This is needed to compare compiled methods."

	| numLits literal1 literal2 |
	(numLits := self numLiterals) ~= method numLiterals
		ifTrue: [ ^ false ].
	"The last literal requires special checking instead of using #literalEqual:"
	1 to: numLits - 1 do: [ :index | 
		literal1 := self literalAt: index.
		literal2 := method literalAt: index.
		(literal1 == literal2 or: [ literal1 literalEqual: literal2 ])
			ifFalse: [ 
				(index = 1 and: [ self isNamedPrimitive | self isExternalCallPrimitive ])
					ifTrue: [ 
						literal1 isArray
							ifTrue: [ 
								(literal2 isArray and: [ literal1 allButLast = literal2 allButLast ])
									ifFalse: [ ^ false ] ]
							ifFalse: [ 
								"ExternalLibraryFunction"
								(literal1 analogousCodeTo: literal2)
									ifFalse: [ ^ false ] ] ]
					ifFalse: [ 
						index = (numLits - 1)
							ifTrue: [ 
								"properties"
								"don't create properties if they don't exist"
								(literal1 isSymbol and: [ literal2 isSymbol ]) ifFalse: [
									(self properties analogousCodeTo: method properties) ifFalse: [ ^false ] ] ]
							ifFalse: [ ^ false ] ] ] ].
	"Class side methods have non unique (nil -> a Metaclass) as literal and cannot be compared equal"
	literal1 := self literalAt: numLits.
	literal2 := method literalAt: numLits.
	^literal1 class == literal2 class
	     and: [literal1 isVariableBinding
	                ifTrue: [literal1 key = literal2 key and: [literal1 value = literal2 value]]
	                ifFalse: [literal1 = literal2]]