testing
testExpiring
	| cache didComputeValue |
	cache := self newCache.
	cache timeToLive: 1 second.
	didComputeValue := false.
	cache at: #foo ifAbsentPut: [ didComputeValue := true. 100 ].
	self assert: didComputeValue.
	self assert: (cache includesKey: #foo).
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 100.
	1 second asDelay wait.
	didComputeValue := false.
	cache at: #foo ifAbsentPut: [ didComputeValue := true. 101 ].
	self assert: didComputeValue.
	self assert: (cache includesKey: #foo).
	self assert: (cache at: #foo ifAbsentPut: [ self fail ]) equals: 101.
	cache validateInvariantWith: self 
	