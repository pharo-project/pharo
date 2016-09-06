tests
testCorrespondingTestedMethods
	{(self stubTestClass >> #testOne) ->
		{(self stubClass >> #one).
		(self stubClass >> #one:)}.
	(self stubTestClass >> #testTwo) ->
		{(self stubClass >> #two).
		(self stubClass >> #two:)}.
	(self stubTestClass >> #testOneTwo) ->
		{(self stubClass >> #one:two:).
		(self stubClass >> #oneTwo:).
		(self stubClass class >> #one:two:)}.
	(self stubTestClass >> #stubClass) ->
		{}.
	(self stubTestClass >> #newStub) ->
		{}} do: [ :each |
		self assert: each key correspondingTestedMethods asSet = each value asSet].

	"non-test methods should have no corresponding tested methods"
	self stubClass methods , self stubClass class methods do: [ :each | 
			self assert: each correspondingTestedMethods isEmpty ]