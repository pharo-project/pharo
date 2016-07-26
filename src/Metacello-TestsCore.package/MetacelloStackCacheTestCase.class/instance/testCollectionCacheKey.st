tests
testCollectionCacheKey

	| x |
	self assert: 
		(Array with: self class with: (Array with: 'faux' with: 'foobar'))
			= (Array with: self class with: (Array with: 'faux' with: 'foobar')).
	self deny: 
		(Array with: self class with: (Array with: 'faux' with: 'foobar'))
			= (Array with: self class with: (Array with: 'faux' with: 'foobar' with: 'foobaz')).
	self assert: (x := self collectionCacheKey) == 6.
