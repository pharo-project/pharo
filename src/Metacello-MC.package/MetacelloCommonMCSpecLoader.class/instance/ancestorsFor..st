versionInfo
ancestorsFor: packageSpec

	| cacheKey vi |
	cacheKey := packageSpec file.
	^MetacelloPlatform current
		stackCacheFor: #ancestors
		at: cacheKey
		doing: [ :cache | 
			vi := packageSpec ancestors.
			cache at: cacheKey put: vi ].