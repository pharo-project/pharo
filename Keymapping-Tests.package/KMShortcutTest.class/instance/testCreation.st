tests
testCreation
	self assert: ($e asKeyCombination isKindOf: KMSingleKeyCombination).
	self assert: ($e ctrl isKindOf: KMModifiedKeyCombination).
	self assert: ($e ctrl alt shift command isKindOf: KMModifiedKeyCombination).