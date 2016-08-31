tests
testCorrespondingTestMethod
	self
		assert: (self stubClass >> #one) correspondingTestMethod == (self stubTestClass >> #testOne);
		assert: (self stubClass >> #untestedOne) correspondingTestMethod isNil;
		assert: (self stubClass >> #one:) correspondingTestMethod == (self stubTestClass >> #testOne);
		assert: (self stubClass >> #untestedOne:) correspondingTestMethod isNil;
		assert: (self stubClass >> #two) correspondingTestMethod == (self stubTestClass >> #testTwo);
		assert: (self stubClass >> #untestedTwo) correspondingTestMethod isNil;
		assert: (self stubClass >> #two:) correspondingTestMethod == (self stubTestClass >> #testTwo);
		assert: (self stubClass >> #untestedTwo:) correspondingTestMethod isNil;
		assert: (self stubClass >> #one:two:) correspondingTestMethod == (self stubTestClass >> #testOneTwo);
		assert: (self stubClass >> #untestedOne:two:) correspondingTestMethod isNil;
		assert: (self stubClass >> #oneTwo:) correspondingTestMethod == (self stubTestClass >> #testOneTwo);
		assert: (self stubClass >> #untestedOneTwo:) correspondingTestMethod isNil.
	self
		assert: (self stubClass class >> #one:two:) correspondingTestMethod == (self stubTestClass >> #testOneTwo);
		assert: (self stubClass class >> #untestedOne:two:) correspondingTestMethod isNil.
	self
		assert: self stubTestClass >> #testOne == (self stubTestClass >> #testOne);
		assert: self stubTestClass >> #testTwo == (self stubTestClass >> #testTwo);
		assert: self stubTestClass >> #testOneTwo == (self stubTestClass >> #testOneTwo)