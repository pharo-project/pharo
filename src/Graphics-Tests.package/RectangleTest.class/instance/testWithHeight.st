tests - rectangle functions
testWithHeight

	| r |
	r := Rectangle new.
	r := 1 @ 2 corner: 5 @ 5.
	self assert: (r withHeight: 7) = (1 @ 2 corner: 5 @ 9).
	self assert: (r withHeight: 3) = (1 @ 2 corner: 5 @ 5).