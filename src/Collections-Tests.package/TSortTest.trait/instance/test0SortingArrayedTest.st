tests - fixture
test0SortingArrayedTest
	| tmp sorted |
	" an unsorted collection of number "
	self unsortedCollection.
	self unsortedCollection do: [ :each | each isNumber ].
	sorted := true.
	self unsortedCollection
		pairsDo: [ :each1 :each2 | 
			each2 < each1
				ifTrue: [ sorted := false ] ].
	self assert: sorted = false.	" a collection of number sorted in an ascending order"
	self sortedInAscendingOrderCollection.
	self sortedInAscendingOrderCollection do: [ :each | each isNumber ].
	tmp := self sortedInAscendingOrderCollection at: 1.
	self sortedInAscendingOrderCollection
		do: [ :each | 
			self assert: each >= tmp.
			tmp := each ]