tests
testRightBottomQuadrant
	| lf rectangle |
	lf := LayoutFrame new
			leftFraction: 1/2 offset: 1;
			topFraction: 1/2 offset: 1;
			rightFraction: 1 offset: -2;
			bottomFraction: 1 offset: -2;
			yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (101@41 corner: 148@68) = rectangle