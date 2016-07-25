private
cacheInterestingSymbolsAndDo: aBlock
	"For time issue, we collect and perform the block in only one iteration"
	| cache |
	cache := OrderedCollection new: 5000.
	self interestingSymbolsDo: [:symbol |
		cache add: symbol.
		aBlock value: symbol ].
	" only assign the cache in the end, aBlock might do a non-local return!"
	cachedSymbols := cache.