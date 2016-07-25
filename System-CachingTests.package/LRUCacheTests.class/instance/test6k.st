testing
test6k
	| cache |
	(cache := self newCache)
		maximumWeight: 600.
	1 to: 6000 do: [ :each |
		cache at: each ifAbsentPut: [ each * 2 ] ].
	self assert: cache size equals: 600.
	self assert: (cache includesKey: 5401).
	self deny: (cache includesKey: 5400).
	cache validateInvariantWith: self