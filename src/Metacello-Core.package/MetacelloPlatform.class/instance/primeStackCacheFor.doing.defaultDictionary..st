caching
primeStackCacheFor: cacheName doing: noArgBlock defaultDictionary: aDictionary

	self deprecated: 'use #primeStackCacheWith:doing:'.
	self 
		useStackCacheDuring: [:dict | | cache |
			cache := dict at: cacheName ifAbsent: [].
			cache == nil
				ifTrue: [ 
					cache := Dictionary new.
					dict at: cacheName put: cache ].
			^noArgBlock value ] 
		defaultDictionary: aDictionary