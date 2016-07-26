tests
testSortedCollectionWithInstanceVariableReferenceChange
	"IMPORTANT: this test is to demonstrate a limitation.  Unfortunatly there is nothing we can do to solve this problem. The sortBlock was serialized with a method context with different receiver, different temps, etc.
	Tests serialization of a SortedCollection whose sortBlock has a reference to the outer context. 
	Note: Equality can not be used to assert, since its behavior is this:
	| x y |
	x := SortedCollection sortBlock: [:a :b | a >= b ].
	y := SortedCollection sortBlock: [:a :b | a >= b ].
	self assert: x ~= y"

	| aSortedCollection materialized |
	instanceVariableForTesting := false.
	aSortedCollection := self sortedBlockClosureWithInstanceVariable.

	materialized := self resultOfSerializeAndMaterialize: aSortedCollection.

	instanceVariableForTesting := true.
	materialized addAll: #(2 3 1).
	aSortedCollection addAll: #(2 3 1).

	"IMPORTANT: Unfortunatly there is nothing we can do to solve this problem. The sortBlock was serialized with a method context with different values."
	self deny: aSortedCollection asArray = materialized asArray.