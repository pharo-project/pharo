private
updateCachedVersionFromFileName: aString with: aVersion
	self resizeCache: cache.
	(aVersion notNil and: [aVersion isCacheable]) ifTrue: [cache at: aString put: aVersion].
