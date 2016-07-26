private - updating
shouldRefresh
	|newCache result|

	newCache := self generateCacheFor: self entity.
	result := cachedHashes ~= newCache.
	cachedHashes := newCache.
	^ result