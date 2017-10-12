tests - dictionary keys values associations access
testKeysSortedSafely
	| collection result |
	collection := self nonEmpty.
	result := collection keysSortedSafely.
	result do: [ :key | collection at: key ].
	self assert: result size = collection size.
	self should: [ result detect: [ :each | (result occurrencesOf: each) > 1 ] ] raise: Error.
	self assert: result asArray isSorted