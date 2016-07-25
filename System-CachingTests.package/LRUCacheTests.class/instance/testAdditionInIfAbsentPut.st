testing
testAdditionInIfAbsentPut
	| cache |
	cache := self newCache.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	"Add an entry for bar in the block computing the value to add"
	cache at: #bar ifAbsentPut: [ cache at: #bar ifAbsentPut: [ 200 ] ].
	self assert: (cache at: #bar ifAbsentPut: [ self fail ]) equals: 200.
	cache validateInvariantWith: self