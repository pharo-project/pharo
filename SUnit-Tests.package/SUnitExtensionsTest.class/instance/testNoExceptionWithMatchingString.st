tests
testNoExceptionWithMatchingString
	self 
		shouldnt: [ Object messageNotUnderstoodByObject ]
		raise: MessageNotUnderstood 
		whoseDescriptionIncludes: 'Zero'
		description: 'tested obsoleting Object'