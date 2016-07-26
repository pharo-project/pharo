testing
testDoubleAccessors
	| alien |
	alien := Alien newC: 8. 
	alien doubleAt: 1 put: 8.8.
	self should: [(alien doubleAt: 1) = 8.8].
		alien free.