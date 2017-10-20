testing
testFactory
	| cache |
	cache := self newCache.
	cache maximumWeight: 5.
	cache factory: [ :key | key * 2 ].
	#( 1 2 3 4 1 5 6 7 8 1 ) do: [ :each |
		 cache at: each ].
	self assert: cache size equals: 5.
	self assert: cache hits equals: 2.
	cache validateInvariantWith: self