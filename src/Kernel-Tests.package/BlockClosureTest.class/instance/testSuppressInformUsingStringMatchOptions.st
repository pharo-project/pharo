tests
testSuppressInformUsingStringMatchOptions

	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('Should not see this message or this test failed!')) isNil].
	
	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('not see this message')) isNil].
	
	self should: [([nil inform: 'Should not see this message or this test failed!'] valueSuppressingMessages: #('*message*failed#')) isNil].
