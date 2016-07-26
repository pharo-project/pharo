testing method dictionary
isLocalAliasSelector: aSymbol
	"Return true if the selector aSymbol is an alias defined
	in my trait composition."

	^(self includesLocalSelector: aSymbol) not
		and: [self hasTraitComposition
				and: [self traitComposition isLocalAliasSelector: aSymbol]]