"
SUnit test for TestCase class
"
Class {
	#name : 'TestCaseTest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'events' }
TestCaseTest >> testAnnouncement [

	| announcements oldCollection suite unitTest result |
	unitTest := ClassFactoryForTestCaseTest.
	announcements := Dictionary new.
	unitTest resetAnnouncer.

	self deny: unitTest shouldAnnounce.
	self deny: unitTest new shouldAnnounce.

	unitTest announcer when: TestCaseAnnouncement do: [ :ann | (announcements at: ann class ifAbsentPut: [ OrderedCollection new ]) add: ann ] for: self.

	self assert: unitTest shouldAnnounce.
	self assert: unitTest new shouldAnnounce.

	"We run SUnitTest"
	suite := unitTest buildSuite.
	result := suite run.

	self assertEmpty: result failures.
	self assertEmpty: result errors.

	self
		assert: (announcements at: TestCaseStarted ifAbsent: [ self fail: 'We should have received some TestCaseStarted announcements.' ]) size
		equals: suite tests size.
	self
		assert: (announcements at: TestCaseEnded ifAbsent: [ self fail: 'We should have received some TestCaseEnded announcements.' ]) size
		equals: suite tests size.
	self assertCollection: announcements keys hasSameElements: {
			TestCaseStarted.
			TestCaseEnded }.

	oldCollection := announcements copy.
	unitTest resetAnnouncer.
	unitTest debug: #testClassCreationInDifferentTags.
	self assert: announcements equals: oldCollection
]

{ #category : 'testing' }
TestCaseTest >> testAssertEqualsActualAndExpectedOrder [
	self
		should: [
			self assert: 'actual' equals: 'expected with fairly long print string'
		]
		raise: TestFailure
		whoseDescriptionIncludes: 'Got ''actual'' instead of ''expected with fairly long print string''.'
		description: 'In #assert:equals: the first argument is the actual value and the second one is the expected.'
]
