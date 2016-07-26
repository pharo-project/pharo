running
availableFor: aTestAsserter
	aTestAsserter
		assert: self isAvailable
		description:
			['Unavailable resource ' , self name , ' requested by '
				, aTestAsserter printString]