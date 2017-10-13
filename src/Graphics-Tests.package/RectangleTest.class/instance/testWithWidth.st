tests - rectangle functions
testWithWidth

	| r |
	r := Rectangle new.
	r := 1 @ 2 corner: 5 @ 5.
	self assert: (r withWidth: 7) = (1 @ 2 corner: 8 @ 5).
	self assert: (r withWidth: 3) = (1 @ 2 corner: 4 @ 5).