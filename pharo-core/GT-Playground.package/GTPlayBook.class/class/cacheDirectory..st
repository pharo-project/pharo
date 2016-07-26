accessing
cacheDirectory: aStringOrFileReference

	cacheDirectory := self 
							ensureDirectory: aStringOrFileReference 
							distinctFrom: self defaultCacheDirectory