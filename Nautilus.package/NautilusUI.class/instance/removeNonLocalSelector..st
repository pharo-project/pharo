menus behavior
removeNonLocalSelector: aSymbol
	| traits isAlias |
	traits := self selectedClassOrMetaClass traitsProvidingSelector: aSymbol.
	isAlias := self selectedClassOrMetaClass isLocalAliasSelector: aSymbol.
	isAlias
		ifTrue: [
			[traits size = 1] assert.
			self selectedClassOrMetaClass removeAlias: aSymbol of: traits first]
		ifFalse: [
			traits do: [:each |
				self selectedClassOrMetaClass addExclusionOf: aSymbol to: each ]]
	