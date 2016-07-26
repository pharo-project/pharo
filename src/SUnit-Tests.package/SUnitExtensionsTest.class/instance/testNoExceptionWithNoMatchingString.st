tests
testNoExceptionWithNoMatchingString
	self
		shouldnt: [ Object messageNotUnderstoodByObject ]
		raise: MessageNotUnderstood
		whoseDescriptionDoesNotInclude: 'messageNotUnderstoodByObject'
		description: 'tested obsoleting Object'