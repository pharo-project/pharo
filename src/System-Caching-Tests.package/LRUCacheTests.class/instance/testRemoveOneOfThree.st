testing
testRemoveOneOfThree
	| cache |
	cache := self newCache.
	cache at: #x ifAbsentPut: [ 100 ].
	cache at: #y ifAbsentPut: [ 200 ].
	cache at: #z ifAbsentPut: [ 300 ].
	cache removeKey: #y.
	self assert: cache size equals: 2.
	self assert: cache totalWeight equals: 2.
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self deny: (cache includesKey: #y).
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	cache validateInvariantWith: self