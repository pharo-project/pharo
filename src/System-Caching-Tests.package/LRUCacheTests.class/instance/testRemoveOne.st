testing
testRemoveOne
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache removeKey: #foo.
	self assert: cache isEmpty.
	self assert: cache size isZero.
	self assert: cache totalWeight isZero.
	self deny: (cache includesKey: #foo).
	cache validateInvariantWith: self