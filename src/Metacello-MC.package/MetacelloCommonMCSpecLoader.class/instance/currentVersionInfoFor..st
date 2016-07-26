versionInfo
currentVersionInfoFor: packageSpec

	| cacheKey vi |
	cacheKey := packageSpec file.
	^MetacelloPlatform current
		stackCacheFor: #currentVersionInfo
		at: cacheKey
		doing: [ :cache | 
			vi := packageSpec currentVersionInfo.
			cache at: cacheKey put: vi ].