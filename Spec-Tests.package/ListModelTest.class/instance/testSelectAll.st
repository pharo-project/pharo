tests
testSelectAll
	| list items |
	list := ListModel new.
	items := #(#AAA #BBB #CCC).
	list items: items.
	list beMultipleSelection.
	self assert: list selectedItems equals: #().
	list selectAll.
	self assert: list selectedItems equals: items