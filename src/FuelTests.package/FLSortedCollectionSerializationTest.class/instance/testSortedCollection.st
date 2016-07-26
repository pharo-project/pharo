tests
testSortedCollection
	"Tests that a SortedCollection works fine after materialization. 
	Note: Equality can not be used to assert, since its behavior is this:
	| x y |
	x := SortedCollection sortBlock: [:a :b | a >= b ].
	y := SortedCollection sortBlock: [:a :b | a >= b ].
	self assert: x ~= y"

	| aSortBlock aSortedCollection materialized |
	aSortBlock := [:a :b | a >= b ].
	aSortedCollection := SortedCollection sortBlock: aSortBlock.

	materialized := self resultOfSerializeAndMaterialize: aSortedCollection.
	self assert: aSortedCollection sortBlock ~~ materialized sortBlock.

	materialized addAll: #(2 1 3).
	aSortedCollection addAll: #(2 1 3).	
	self assert: aSortedCollection asArray = materialized asArray.