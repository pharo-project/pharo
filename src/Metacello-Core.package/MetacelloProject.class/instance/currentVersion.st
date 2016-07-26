versions
currentVersion
	| cacheKey cv |
	cacheKey := self configuration class.
	^ MetacelloPlatform current
		stackCacheFor: #currentVersion
		at: cacheKey
		doing: [ :cache | 
			cv := self currentVersionAgainst: nil.
			^ cache at: cacheKey put: cv ]