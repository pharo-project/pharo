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
	| announcements oldCollection suite unitTestClass result |
	
	unitTestClass := ClassFactoryForTestCaseTest.
	announcements := Dictionary new.
	unitTestClass resetAnnouncer.

	self deny: unitTestClass shouldAnnounce.
	self deny: unitTestClass new shouldAnnounce.

	unitTestClass announcer 
		when: TestCaseAnnouncement 
		do: [ :ann | (announcements at: ann class ifAbsentPut: [ OrderedCollection new ]) add: ann ] 
		for: self.

	self assert: unitTestClass shouldAnnounce.
	self assert: unitTestClass new shouldAnnounce.

	"We run SUnitTest"
	suite := unitTestClass buildSuite.
	result := suite run.

	self assertEmpty: result failures.
	self assertEmpty: result errors.

	self
		assert: (announcements at: TestCaseStarted ifAbsent: [ #() ]) size
		equals: suite tests size.
	self
		assert: (announcements at: TestCaseEnded ifAbsent: [ #() ]) size
		equals: suite tests size.
	
	self assertCollection: announcements keys hasSameElements: {
			TestCaseStarted.
			TestCaseEnded }.

	oldCollection := announcements copy.
	unitTestClass resetAnnouncer.
	unitTestClass debug: #testClassCreationInDifferentTags.
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
