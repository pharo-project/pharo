testing
testOneHitTwice
	| cache value |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	value := cache at: #foo ifAbsentPut: [ self fail ].
	self assert: value equals: 100.
	self assert: cache hitRatio equals: 2/3.
	cache validateInvariantWith: self