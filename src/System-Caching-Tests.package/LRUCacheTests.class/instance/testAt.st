testing
testAt
	| cache |
	cache := self newCache.
	self should: [ cache at: #foo ] raise: KeyNotFound.
	cache at: #foo ifAbsentPut: [ 100 ].
	self assert: (cache at: #foo) equals: 100.
	cache removeKey: #foo.
	self should: [ cache at: #foo ] raise: KeyNotFound