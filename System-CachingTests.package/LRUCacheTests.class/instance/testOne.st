testing
testOne
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache includesKey: #foo).
	self deny: cache isEmpty.
	self assert: cache size equals: 1.
	self assert: cache totalWeight equals: 1.
	self assert: cache hits isZero.
	self assert: cache misses equals: 1.
	self assert: cache hitRatio isZero.
	self deny: (cache includesKey: #bar).
	cache validateInvariantWith: self