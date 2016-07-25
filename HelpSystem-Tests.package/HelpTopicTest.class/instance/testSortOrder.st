testing
testSortOrder

	|a b c sorted |
	a := self defaultTestClass named: 'A'.
	b := self defaultTestClass named: 'B'.
	c := self defaultTestClass named: 'C'.
	sorted := (OrderedCollection with: b with: c with: a) asSortedCollection.
	self assert: sorted first = a.
	self assert: sorted last = c.
	