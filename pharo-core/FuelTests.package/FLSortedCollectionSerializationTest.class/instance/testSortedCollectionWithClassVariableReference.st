tests
testSortedCollectionWithClassVariableReference
	"Tests serialization of a SortedCollection whose sortBlock has a reference to the outer context. 
	Note: Equality can not be used to assert, since its behavior is this:
	| x y |
	x := SortedCollection sortBlock: [:a :b | a >= b ].
	y := SortedCollection sortBlock: [:a :b | a >= b ].
	self assert: x ~= y"

	| aSortedCollection materialized |
	ClassVariableForTesting := false.
	
	aSortedCollection := self class sortedCollectionForTestingWithClassVariable.
	materialized := self resultOfSerializeAndMaterialize: aSortedCollection.
	ClassVariableForTesting := true.
	materialized addAll: #(2 3 1).
	aSortedCollection addAll: #(2 3 1).
	
	aSortedCollection sortBlock assertWellMaterializedInto:  materialized sortBlock in: self.
	self assert: aSortedCollection asArray = materialized asArray.