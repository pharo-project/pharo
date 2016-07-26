morphic ui
saveChanges
	| currentRepository workingCopy |
	
	currentRepository := repository. "This can be changed elsewhere while processing"
	workingCopy := MCWorkingCopy forPackage: (MCPackage new name: selectedPackage).
	(workingCopy newVersionIn: currentRepository) ifNotNil: [:v |
		(MCVersionInspector new version: v) show.
		Cursor wait showWhile: [currentRepository storeVersion: v].
		MCCacheRepository uniqueInstance cacheAllFileNamesDuring: 
			[currentRepository cacheAllFileNamesDuring: 
				[v allAvailableDependenciesDo:
					[:dep |
					(currentRepository includesVersionNamed: dep info name)
						ifFalse: [currentRepository storeVersion: dep]]]]]