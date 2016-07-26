tests
testPreviousNext
	| n p |
	n := month next.
	p := month previous.

	self
		assert: n year = 1998;
		assert: n index = 8;
		assert: p year = 1998;
		assert: p index = 6.

