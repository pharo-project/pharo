Class {
	#name : 'TestSuiteTest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
TestSuiteTest >> testAnnouncement [
	| announcements unitTest suiteTest result |
	
	unitTest := ClassFactoryForTestCaseTest.
	unitTest resetAnnouncer.
	
	suiteTest := unitTest suite.
	announcements := Dictionary new.

	suiteTest testSuiteAnnouncer 
		when: TestCaseAnnouncement 
		do: [ :ann | (announcements at: ann class ifAbsentPut: [ OrderedCollection new ]) add: ann ] 
		for: self.

	result := suiteTest run.

	self assertEmpty: result failures.
	self assertEmpty: result errors.

	self
		assert: (announcements at: TestCaseStarted ifAbsent: [ #() ]) size
		equals: suiteTest tests size.
	self
		assert: (announcements at: TestCaseEnded ifAbsent: [ #() ]) size
		equals: suiteTest tests size.
	
	self assertCollection: announcements keys hasSameElements: {
			TestCaseStarted.
			TestCaseEnded.
			TestSuiteEnded }.
			
	"announcer should have been reset after suite execution"
	self deny: unitTest shouldAnnounce
]

{ #category : 'tests' }
TestSuiteTest >> testAnnouncementInCompositeTest [
	| announcements unitTest suiteTest result |
	unitTest := ClassFactoryForTestCaseTest.
	unitTest resetAnnouncer.
	
	suiteTest := TestSuite new 
		addTest: unitTest suite;
		addTest: unitTest suite;
		yourself.
	announcements := Dictionary new.

	suiteTest testSuiteAnnouncer 
		when: TestCaseAnnouncement 
		do: [ :ann | (announcements at: ann class ifAbsentPut: [ OrderedCollection new ]) add: ann ] 
		for: self.

	result := suiteTest run.

	self assertEmpty: result failures.
	self assertEmpty: result errors.

	self
		assert: (announcements at: TestCaseStarted ifAbsent: [ #() ]) size
		equals: (suiteTest tests inject: 0 into: [ :sum :suite | sum + suite tests size ]).
	self
		assert: (announcements at: TestCaseEnded ifAbsent: [ #() ]) size
		equals: (suiteTest tests inject: 0 into: [ :sum :suite | sum + suite tests size ]). 

	self assertCollection: announcements keys hasSameElements: {
			TestCaseStarted.
			TestCaseEnded.
			TestSuiteEnded }.
			
	"announcer should have been reset after suite execution"
	self deny: unitTest shouldAnnounce
]

{ #category : 'tests' }
TestSuiteTest >> testHistory [
	| unitTest suiteTest result |
	
	unitTest := ClassFactoryForTestCaseTest.
	unitTest resetAnnouncer.
	unitTest resetHistory.
	
	suiteTest := unitTest suite.
	result := suiteTest run.

	self deny: unitTest history isEmpty.
	self assert: (unitTest history at: #passed) size equals: 7.
	self assert: (unitTest history at: #failures) size equals: 0.
	self assert: (unitTest history at: #errors) size equals: 0.

	unitTest resetHistory	
]
