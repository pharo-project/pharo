tests
testPreviousNext
	self
		assert: week next = (Week starting: '6 July 1998' asDate);
		assert: week previous = (Week starting:  '22 June 1998' asDate)