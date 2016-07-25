testing
testTwo
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache at: #bar ifAbsentPut: [ 200 ].
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	self assert: cache hitRatio equals: 2/4.
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	self assert: cache hitRatio equals: 4/6.
	cache validateInvariantWith: self