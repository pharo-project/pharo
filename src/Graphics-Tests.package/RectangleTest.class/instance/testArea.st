tests - accessing
testArea

	self assert: (0 @ 0 corner: 5 @ 5) area = 25.
	self assert: (2 @ 3 corner: 10 @ 8) area = 40.
	self assert: (5 @ 5 corner: 6 @ 6) area = 1.
	self assert: (-1 @ -2 corner: 2 @ 1) area = 9.