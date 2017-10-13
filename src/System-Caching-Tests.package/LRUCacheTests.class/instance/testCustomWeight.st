testing
testCustomWeight
	| cache |
	(cache := self newCache)
		computeWeight: #sizeInMemory;
		maximumWeight: 64.
	cache at: 1 ifAbsentPut: [ ByteArray new: 1 ].
	self assert: cache totalWeight equals: (ByteArray new: 1) sizeInMemory.
	2 to: 10 do: [ :each | 
		cache at: each ifAbsentPut: [ ByteArray new: each ] ].
	self assert: cache size equals: 3.
	self 
		assert: cache totalWeight 
		equals: ((8 to: 10) collect: [ :each | (ByteArray new: each) sizeInMemory ]) sum.
	self assert: (cache at: 10 ifAbsentPut: [ self fail ]) equals: (ByteArray new: 10).
	self deny: (cache includesKey: 1)