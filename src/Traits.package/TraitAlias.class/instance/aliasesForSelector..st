enquiries
aliasesForSelector: aSymbol
	| selectors |
	selectors := self aliases
		select: [:association | association value = aSymbol]
		thenCollect: [:association | association key].
	^(super aliasesForSelector: aSymbol)
		addAll: selectors;
		yourself
		 