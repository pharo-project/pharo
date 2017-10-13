tests - rectangle functions
testWithTop

	| r |
	r := Rectangle new.
	r := 0 @ 0 corner: 5 @ 5.
	self assert: (r withTop: -2) = (0 @ -2 corner: 5 @ 5).
	self assert: (r withTop: 3) = (0 @ 3 corner: 5 @ 5).