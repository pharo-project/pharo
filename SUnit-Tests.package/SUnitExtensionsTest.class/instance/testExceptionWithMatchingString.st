tests
testExceptionWithMatchingString
	self should: [ Object obsolete ] raise: Error whoseDescriptionIncludes: 'NOT obsolete' description: 'tested obsoleting Object'