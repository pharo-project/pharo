testing method dictionary
isAliasSelector: aSymbol
	"Return true if the selector aSymbol is an alias defined
	in my or in another composition somewhere deeper in 
	the tree of traits compositions."

	^(self includesLocalSelector: aSymbol) not
		and: [self hasTraitComposition
				and: [self traitComposition isAliasSelector: aSymbol]]