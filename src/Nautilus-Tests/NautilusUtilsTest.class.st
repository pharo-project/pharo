Class {
	#name : #NautilusUtilsTest,
	#superclass : #TestCase,
	#category : 'Nautilus-Tests'
}

{ #category : #accessing }
NautilusUtilsTest >> stubClass [
	^ NautilusUtilsTestStub
]

{ #category : #accessing }
NautilusUtilsTest >> stubTestClass [
	^ NautilusUtilsTestStubTest
]

{ #category : #tests }
NautilusUtilsTest >> testAsTestMethod [
	{((self stubClass >> #one) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #one:) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #two) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #two:) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #oneTwo:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #untestedOne) -> nil).
	((self stubClass >> #untestedOne:) -> nil).
	((self stubClass >> #untestedTwo) -> nil).
	((self stubClass >> #untestedTwo:) -> nil).
	((self stubClass >> #untestedOne:two:) -> nil).
	((self stubClass >> #untestedOneTwo:) -> nil).
	((self stubClass class >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass class >> #untestedOne:two:) -> nil).
	((self stubTestClass >> #testOne) -> (self stubTestClass >> #testOne)).
	((self stubTestClass >> #testTwo) -> (self stubTestClass >> #testTwo)).
	((self stubTestClass >> #testOneTwo) -> (self stubTestClass >> #testOneTwo))}
		do: [ :each | self assert: each key asTestMethod == each value ]
]

{ #category : #tests }
NautilusUtilsTest >> testCorrespondingTestCase [
	"tests that classes and their meta classes have the same test class
	corresponding to them"

	{(Behavior -> BehaviorTest).
	(Behavior class -> BehaviorTest).
	(BehaviorTest -> nil).
	(Class -> ClassTest).
	(Class class -> ClassTest).
	(ClassTest -> nil).
	(NautilusUtilsTestStub -> NautilusUtilsTestStubTest).
	(NautilusUtilsTestStub class -> NautilusUtilsTestStubTest).
	(NautilusUtilsTestStubTest -> nil).
	(NautilusUtilsTestStubUntested -> nil).
	(NautilusUtilsTestStubUntested class -> nil)}
		do: [ :each | self assert: each key correspondingTestCase == each value ]
]

{ #category : #tests }
NautilusUtilsTest >> testCorrespondingTestMethod [
	{((self stubClass >> #one) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #one:) -> (self stubTestClass >> #testOne)).
	((self stubClass >> #two) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #two:) -> (self stubTestClass >> #testTwo)).
	((self stubClass >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #oneTwo:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass >> #untestedOne) -> nil).
	((self stubClass >> #untestedOne:) -> nil).
	((self stubClass >> #untestedTwo) -> nil).
	((self stubClass >> #untestedTwo:) -> nil).
	((self stubClass >> #untestedOne:two:) -> nil).
	((self stubClass >> #untestedOneTwo:) -> nil).
	((self stubClass class >> #one:two:) -> (self stubTestClass >> #testOneTwo)).
	((self stubClass class >> #untestedOne:two:) -> nil).
	((self stubTestClass >> #testOne) -> nil).
	((self stubTestClass >> #testTwo) -> nil).
	((self stubTestClass >> #testOneTwo) -> nil)}
		do: [ :each | self assert: each key correspondingTestMethod == each value ]
]

{ #category : #tests }
NautilusUtilsTest >> testCorrespondingTestedMethods [
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
]

{ #category : #tests }
NautilusUtilsTest >> testNormalizedSelectorForComparison [
	#(#one #one: #two #two: #one:two: #oneTwo: #+ #- #+-)
		with: #('one' 'one' 'two' 'two' 'onetwo' 'onetwo' '+' '-' '+-')
		do: [ :original :transformed | 
			self
				assert: original normalizedSelectorForComparison = transformed;
				assert: original asString normalizedSelectorForComparison = transformed;
				assert: transformed normalizedSelectorForComparison = transformed.

			"should always return a string copy"
			self
				deny: original normalizedSelectorForComparison == original;
				deny: transformed normalizedSelectorForComparison == transformed;
				deny: original normalizedSelectorForComparison isSymbol;
				deny: transformed normalizedSelectorForComparison isSymbol ]
]
