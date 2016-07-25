accessing
cacheRepository

	cacheRepository == nil ifTrue: [ cacheRepository := MCDictionaryRepository new ].
	^ cacheRepository