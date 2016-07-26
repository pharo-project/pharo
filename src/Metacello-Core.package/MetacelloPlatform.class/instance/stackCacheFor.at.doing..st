caching
stackCacheFor: cacheName at: key doing: aBlock

	^self stackCacheFor: cacheName cacheClass: Dictionary at: key doing: aBlock