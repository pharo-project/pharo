private
valuesFor: globalSymbols in: globalEnvironment
	"Some globals, like ActiveHand have a nil value in the Smalltalk globals. Therefore, we cannot map nil to globalCluster. We could filter before in #defaultGlobalSymbols but that means that not even the Association will be consider global."
	| values |
	values := IdentitySet new: globalSymbols size.
	"todo: optimized ugly code"
	globalSymbols 
		do: [:aSymbol | 
			(globalEnvironment 
				at: aSymbol ifPresent: [:value | 
					value ifNotNil: [ 
						values add: value ] ]) ].
	^ values