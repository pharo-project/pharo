tests
testStrings

	| a b c d |
	a := 'xc'.
	b := 'xd'.
	c := 'xd'.
	d := 'xa'.
	self assert: (b threeWayCompareTo: c) isZero.
	self assert: (a threeWayCompareTo: b) equals: -1.
	self assert: (b threeWayCompareTo: a) equals: 1.
	self assert: (a threeWayCompareTo: d) equals: 1.
	self assert: (d threeWayCompareTo: a) equals: -1