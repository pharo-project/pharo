instance creation
named: aSymbol uses: aTraitCompositionOrCollection category: aString
	| env |
	env := self environment.
	^self
		named: aSymbol
		uses: aTraitCompositionOrCollection
		category: aString
		env: env