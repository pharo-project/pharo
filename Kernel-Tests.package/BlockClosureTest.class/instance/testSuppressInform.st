tests
testSuppressInform

	self should: [[nil inform: 'Should not see this message or this test failed!'] valueSuppressingAllMessages isNil]