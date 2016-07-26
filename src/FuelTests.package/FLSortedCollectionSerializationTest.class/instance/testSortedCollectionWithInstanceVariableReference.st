tests
testSortedCollectionWithInstanceVariableReference
	"Tests serialization of a SortedCollection whose sortBlock has a reference to the outer context. 
	Note: Equality can not be used to assert, since its behavior is this:
	| x y |
	x := SortedCollection sortBlock: [:a :b | a >= b ].
	y := SortedCollection sortBlock: [:a :b | a >= b ].
	self assert: x ~= y"

	| aSortedCollection materialized |
	instanceVariableForTesting := false.
	aSortedCollection := self sortedBlockClosureWithInstanceVariable.
	
	materialized := self resultOfSerializeAndMaterialize: aSortedCollection.
	materialized addAll: #(2 3 1).
	aSortedCollection addAll: #(2 3 1).
	
	self assert: aSortedCollection asArray = materialized asArray.
	self assert: aSortedCollection asArray = #(3 2 1)