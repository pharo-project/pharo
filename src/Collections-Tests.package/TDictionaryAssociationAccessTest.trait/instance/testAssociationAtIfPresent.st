tests - dictionary assocition access
testAssociationAtIfPresent

	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn ifPresent: [:assoc | {#present. assoc}].
	self
		assert: result isArray;
		assert: result size = 2;
		assert: result first = #present;
		assert: result second key = keyIn;
		assert: result second value = (collection at: keyIn).

	"test that cull: is used to support zero-arg blocks"
	result := collection associationAt: keyIn ifPresent: [#present].
	self assert: result = #present.

	result := collection associationAt: self keyNotIn ifPresent: [:assoc | {#present. assoc}].
	self assert: result isNil.