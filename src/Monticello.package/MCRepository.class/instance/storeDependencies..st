storing
storeDependencies: aVersion
	MCCacheRepository uniqueInstance cacheAllFileNamesDuring: 
		[self cacheAllFileNamesDuring: 
			[aVersion allAvailableDependenciesDo:
				[:dep |
					(self includesVersionNamed: dep info name)
						ifFalse: [self storeVersion: dep]]]]