tests
testSelectAll
	| items |
	items := #(#AAA #BBB #CCC).
	testedInstance items: items.
	testedInstance beMultipleSelection.
	self assert: testedInstance selectedItems equals: #().
	testedInstance selectAll.
	self assert: testedInstance selectedItems equals: items