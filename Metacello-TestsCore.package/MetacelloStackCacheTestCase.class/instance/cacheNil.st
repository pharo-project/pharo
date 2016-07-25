tests
cacheNil

	^MetacelloPlatform current 
		stackCacheFor: #cacheNil
		at: #key 
		doing: [:cache | | value |
			cache at: #key put: nil.
			self cacheNil ]
