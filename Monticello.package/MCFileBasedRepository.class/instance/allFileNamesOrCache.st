accessing
allFileNamesOrCache
	cacheFileNames == true ifFalse: [ ^ self allFileNames ].
	^ allFileNames ifNil: [
			allFileNames := self allFileNames]