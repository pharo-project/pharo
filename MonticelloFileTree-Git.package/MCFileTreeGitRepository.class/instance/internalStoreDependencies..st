storing
internalStoreDependencies: aVersion
	| deps |
	deps := OrderedCollection new.
	MCCacheRepository uniqueInstance
		cacheAllFileNamesDuring: [ 
			self
				cacheAllFileNamesDuring: [ 
					aVersion
						allAvailableDependenciesDo: [ :dep | 
							(self includesVersionNamed: dep info name)
								ifFalse: [ 
									self internalStoreVersion: dep.
									deps add: dep ] ] ] ].
	^ deps