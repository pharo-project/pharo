loading
expandToLoadableSpecNames: nameList

	| cacheKey names |
	cacheKey := Array with: self label with: nameList.
	^MetacelloPlatform current
		stackCacheFor: #loadableSpecNames
		at: cacheKey
		doing: [ :cache | 
			names := (self resolveToLoadableSpecs: nameList) collect: [:spec | spec name ].
			cache at: cacheKey put: names ].