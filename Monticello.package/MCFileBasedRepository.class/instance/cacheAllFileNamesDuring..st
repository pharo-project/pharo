caching
cacheAllFileNamesDuring: aBlock
	cacheFileNames == true ifTrue: [ ^ aBlock value ].
	allFileNames := nil.
	cacheFileNames := true.	
	^ aBlock ensure: [
		allFileNames := nil.
		cacheFileNames := false]