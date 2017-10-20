tests - rectangle functions
testWithBottom

	| r |
	r := Rectangle new.
	r := (0 @ 0) corner: (5 @ 5).
	self assert: (r withBottom: 7) = ((0 @ 0) corner: (5 @ 7)).
	self assert: (r withBottom: 3) = ((0 @ 0) corner: (5 @ 3)).

	r := Rectangle new.
	r := (-5 @ -10) corner: (-2 @ -3).
	self assert: (r withBottom: -2) = ((-5 @ -10) corner: (-2 @ -2)).
	self assert: (r withBottom: 5) = ((-5 @ -10) corner: (-2 @ 5)).