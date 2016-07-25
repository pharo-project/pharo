testing
testRemoveStaleValues
	| cache keys |
	cache := self newCache.
	cache timeToLive: 1 second.
	cache at: #foo ifAbsentPut: [ 100 ].
	cache at: #bar ifAbsentPut: [ 200 ].
	self deny: cache isEmpty.
	1 second asDelay wait.
	keys := cache removeStaleValues.
	self assert: cache isEmpty.
	self assert: keys asArray sorted equals: #(bar foo).
	cache validateInvariantWith: self 
	