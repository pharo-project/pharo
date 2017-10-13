tests - rectangle functions
testPointNearestTo

	| rectangle negativeRectangle |
	rectangle := 10 @ 10 corner: 20 @ 20.
	self assert: 10 @ 10 equals: (rectangle pointNearestTo: 9 @ 9).
	self assert: 20 @ 20 equals: (rectangle pointNearestTo: 21 @ 21).
	self assert: 10 @ 20 equals: (rectangle pointNearestTo: 9 @ 21).
	self assert: 20 @ 10 equals: (rectangle pointNearestTo: 21 @ 10).
	self assert: 10 @ 15 equals: (rectangle pointNearestTo: 11 @ 15).
	self assert: 15 @ 10 equals: (rectangle pointNearestTo: 15 @ 13).
	self assert: 20 @ 15 equals: (rectangle pointNearestTo: 17 @ 15).
	self assert: 15 @ 20 equals: (rectangle pointNearestTo: 15 @ 18).
	negativeRectangle := -10 @ -10 corner: -20 @ -20.
	self assert: -10 @ -10 equals: (negativeRectangle pointNearestTo: 0@0).