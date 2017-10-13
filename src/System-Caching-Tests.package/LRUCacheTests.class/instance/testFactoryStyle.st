testing
testFactoryStyle
	| cache factory |
	cache := self newCache.
	cache maximumWeight: 5.
	factory := [ :key | key * 2 ].
	#( 1 2 3 4 1 5 6 7 8 1 ) do: [ :each |
		 cache at: each ifAbsentPut: factory ].
	self assert: cache size equals: 5.
	self assert: cache hits equals: 2.
	cache validateInvariantWith: self