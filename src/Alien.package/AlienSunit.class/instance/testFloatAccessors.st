testing
testFloatAccessors
	| alien |
	alien := Alien newC: 4. 
	alien floatAt: 1 put: 8.8.
	self should: [(alien floatAt: 1) closeTo: 8.8].
		alien free.