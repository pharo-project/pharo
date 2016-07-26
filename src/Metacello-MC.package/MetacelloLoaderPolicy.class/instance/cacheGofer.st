accessing
cacheGofer

	cacheGofer == nil 
		ifTrue: [ 
			"don't use a caching Gofer here, since we expect the contents to change during a fetch operation"
			cacheGofer := Gofer new. 
			cacheGofer disablePackageCache.
			cacheGofer repository: self cacheRepository. ].
	^ cacheGofer