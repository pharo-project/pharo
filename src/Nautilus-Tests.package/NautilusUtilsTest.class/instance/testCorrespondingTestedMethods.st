tests
testCorrespondingTestedMethods
	self
		assert:
			(self stubTestClass >> #testOne) correspondingTestedMethods asSet
				=
					{(self stubClass >> #one).
					(self stubClass >> #one:)} asSet.
	self
		assert:
			(self stubTestClass >> #testTwo) correspondingTestedMethods asSet
				=
					{(self stubClass >> #two).
					(self stubClass >> #two:)} asSet.
	self
		assert:
			(self stubTestClass >> #testOneTwo) correspondingTestedMethods asSet
				=
					{(self stubClass >> #one:two:).
					(self stubClass >> #oneTwo:).
					(self stubClass class >> #one:two:)} asSet.
	#(stubClass newStub)
		do: [ :each |
			self assert: (self stubTestClass >> each) correspondingTestedMethods isEmpty ].

	self stubClass selectors
		do: [ :each | 
			self assert: (self stubClass >> each) correspondingTestedMethods isEmpty ].
	self stubClass class selectors
		do: [ :each | 
			self assert: (self stubClass >> each) correspondingTestedMethods isEmpty ]