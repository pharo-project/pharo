testing
testEviction
	| cache |
	cache := self newCache.
	cache maximumWeight: 16.
	1 to: 20 do: [ :each |
		cache at: each asWords ifAbsentPut: [ each ] ].
	self assert: cache size equals: 16.
	self assert: cache totalWeight equals: 16.
	5 to: 20 do: [ :each |
		self assert: (cache includesKey: each asWords).
		self 
			assert: (cache at: each asWords ifAbsentPut: [ self fail ]) 
			equals: each ].
	cache validateInvariantWith: self