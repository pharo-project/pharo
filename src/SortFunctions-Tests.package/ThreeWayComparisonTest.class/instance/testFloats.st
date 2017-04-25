tests
testFloats

	| a b c d |
	a := 13.1.
	b := 18.9.
	c := 18.9.
	d := -7.2.
	self assert: (b threeWayCompareTo: c) isZero.
	self assert: (a threeWayCompareTo: b) equals: -1.
	self assert: (b threeWayCompareTo: a) equals: 1.
	self assert: (a threeWayCompareTo: d) equals: 1.
	self assert: (d threeWayCompareTo: a) equals: -1