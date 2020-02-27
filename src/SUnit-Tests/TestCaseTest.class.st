"
SUnit test for TestCase class
"
Class {
	#name : #TestCaseTest,
	#superclass : #TestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #events }
TestCaseTest >> testAnnouncement [
	| collection oldCollection suite unitTest |
	unitTest := ClassFactoryForTestCaseTest.
	collection := OrderedCollection new.
	unitTest resetAnnouncer.

	self deny: unitTest shouldAnnounce.
	self deny: unitTest new shouldAnnounce.

	unitTest announcer when: TestCaseAnnouncement do: [ :ann | collection add: ann ].

	self assert: unitTest shouldAnnounce.
	self assert: unitTest new shouldAnnounce.

	"We run SUnitTest"
	suite := unitTest buildSuite.
	suite run.

	self assert: collection size equals: suite tests size * 2.
	self assert: (collection select: [ :c | c isKindOf: TestCaseStarted ]) size equals: collection size / 2.
	self assert: (collection select: [ :c | c isKindOf: TestCaseEnded ]) size equals: collection size / 2.
	self assert: (collection allSatisfy: #hasPassed).
	self deny: (collection anySatisfy: #hasFailures).

	oldCollection := collection copy.
	unitTest resetAnnouncer.
	unitTest debug: #testClassCreationInDifferentCategories.
	self assert: collection equals: oldCollection
]

{ #category : #testing }
TestCaseTest >> testAssertEqualsActualAndExpectedOrder [
	self 
		should: [
			self assert: 'actual' equals: 'expected with fairly long print string'
		]
		raise: TestFailure 
		whoseDescriptionIncludes: 'Got ''actual'' instead of ''expected with fairly long print string''.' 
		description: 'In #assert:equals: the first argument is the actual value and the second one is the expected.'.
]

{ #category : #testing }
TestCaseTest >> testIsUnitTest [

	self assert: TestCase isUnitTest
]
