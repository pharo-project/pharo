tests
testDefaultDictionary

	| defaultDictionary |
	defaultDictionary := Dictionary new.
	MetacelloPlatform current 
		useStackCacheDuring: [:dict | 
			self 
				cachedReturnOfValue;
				collectionCacheKey;
				cacheNil;
				directReturnOfValue;
				mixedStack;
				mixedStackCall ] 
		defaultDictionary: defaultDictionary.
	self assert: (defaultDictionary includesKey: #cached).
	self assert: (defaultDictionary includesKey: #cacheNil).
	self assert: (defaultDictionary includesKey: #collection).
	self assert: (defaultDictionary includesKey: #direct).
	self assert: (defaultDictionary includesKey: #mixed).
	self assert: (defaultDictionary includesKey: #mixedStack).
