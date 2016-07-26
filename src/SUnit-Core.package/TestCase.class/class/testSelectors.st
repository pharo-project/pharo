accessing
testSelectors
	^(self selectors select: [ :each | (each beginsWith: 'test') and: [each numArgs isZero]])