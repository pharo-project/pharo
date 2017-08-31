private
addTestsIn: aTestAsserter to: aSet
	(aTestAsserter isKindOf: TestSuite) ifTrue: [
		aTestAsserter tests
			do: [ :each | self addTestsIn: each to: aSet ] ].
	(aTestAsserter isKindOf: TestCase) ifTrue: [
		(aTestAsserter class respondsTo: #packageNamesUnderTest) ifTrue: [
			aTestAsserter class packageNamesUnderTest
				do: [ :each | aSet add: (RPackage organizer packageNamed: each) ] ] ].
	^ aSet