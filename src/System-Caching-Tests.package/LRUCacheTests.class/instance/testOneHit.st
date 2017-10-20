testing
testOneHit
	| cache value |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache includesKey: #foo).
	self assert: cache hits equals: 0.
	self assert: cache misses equals: 1.
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	self assert: cache hits equals: 1.
	self assert: cache misses equals: 1.
	cache validateInvariantWith: self
	