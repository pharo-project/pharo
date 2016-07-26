tests
testInstanceCreation
	| m1 m2 |
	m1 := Month starting:  '4 July 1998' asDate.
	m2 := Month year: 1998 month: #July .
	self
		assert: month = m1;
		assert: month = m2