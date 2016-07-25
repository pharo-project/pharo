testing
testThreeHitSameOne
	| cache |
	cache := self newCache.
	cache at: #x ifAbsentPut: [ 100 ].
	cache at: #y ifAbsentPut: [ 200 ].
	cache at: #z ifAbsentPut: [ 300 ].
	self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100.
	self assert: (cache at: #y ifAbsentPut: [ self fail ]) equals: 200.
	self assert: (cache at: #z ifAbsentPut: [ self fail ]) equals: 300.
	3 timesRepeat: [ 	
		self assert: (cache at: #x ifAbsentPut: [ self fail ]) equals: 100 ].
	self assert: cache hits equals: 6.
	cache validateInvariantWith: self