testing
testRemoveAll
	| cache |
	cache := self newCache.
	1 to: 10 do: [ :each | 
		cache at: each ifAbsentPut: [ each ] ].
	cache removeAll.
	self assert: cache isEmpty.
	self assert: cache size isZero.
	self assert: cache totalWeight isZero.
	self assert: cache misses equals: 10.
	self assert: cache hits isZero.
	self deny: (cache includesKey: 1).
	cache keysAndValuesDo: [ :key :value | self fail ].
	cache validateInvariantWith: self