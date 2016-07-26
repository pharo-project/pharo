tests-cycles
testCyclicIdentitySet

	| aSet materializedSet |
	aSet := IdentitySet new.
	aSet add: aSet.
	
	materializedSet := self resultOfSerializeAndMaterialize: aSet.
	self assert: aSet ~~ materializedSet .
	self assert: (materializedSet includes: materializedSet) description: 'The materialized set has to include himself. Note aSet = materializedSet is false, bacause equality implementation checks that materializedSet *identity-includes* each element of aSet, which is false.'