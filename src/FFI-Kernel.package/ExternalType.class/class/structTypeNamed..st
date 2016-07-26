private
structTypeNamed: aSymbol
	aSymbol == nil ifTrue:[^nil].
	^self newTypeNamed: aSymbol force: false