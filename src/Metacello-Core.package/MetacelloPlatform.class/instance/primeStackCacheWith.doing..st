caching
primeStackCacheWith: aDictionary doing: noArgBlock

	self 
		useStackCacheDuring: [:dict | ^noArgBlock value ] 
		defaultDictionary: aDictionary