caching
resizeCache: aDictionary
	[aDictionary size <= self maxCacheSize] whileFalse:
		[aDictionary removeKey: aDictionary keys atRandom]