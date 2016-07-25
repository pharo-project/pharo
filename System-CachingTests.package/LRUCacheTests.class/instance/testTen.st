testing
testTen
	| cache |
	cache := self newCache.
	1 to: 10 do: [ :each |
		cache at: each asWords ifAbsentPut: [ each ] ].
	self assert: cache size equals: 10.
	1 to: 10 do: [ :each |
		self assert: (cache includesKey: each asWords) ].
	1 to: 10 do: [ :each |
		self 
			assert: (cache at: each asWords ifAbsentPut: [ self fail ]) 
			equals: each ].
	self assert: cache hitRatio equals: 1/2.
	cache validateInvariantWith: self