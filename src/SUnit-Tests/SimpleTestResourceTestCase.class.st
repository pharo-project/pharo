"
SUnit tests for simple test resources
"
Class {
	#name : #SimpleTestResourceTestCase,
	#superclass : #TestCase,
	#instVars : [
		'resource'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #history }
SimpleTestResourceTestCase class >> lastStoredRun [
	^ ((Dictionary new) add: (#passed->((Set new) add: #testResourceInitRelease; add: #testResourcesCollection; yourself)); add: (#timeStamp->'22 November 2008 10:11:35 pm'); add: (#failures->((Set new))); add: (#errors->((Set new))); yourself)
]

{ #category : #accessing }
SimpleTestResourceTestCase class >> resources [
	^Array with: SimpleTestResource
]

{ #category : #utilities }
SimpleTestResourceTestCase >> clearOuterResourceStateDuring: aBlock [
	"This self-testing test must clear the outer state of its resources before starting and after finishing, so that it can construct test cases and suites of itself and test them."
	
	self
		assert: SimpleTestResource isAlreadyAvailable
		description: 'The resource was not set up for the test'.
	SimpleTestResource reset.
	self
		deny: SimpleTestResource isAlreadyAvailable
		description: 'The resource was still set up before we began the run'.
	^aBlock
		ensure:
			[self
				deny: SimpleTestResource isAlreadyAvailable
				description: 'The resource was still set up after we finished the run'.
			SimpleTestResource isAvailable.
			self
				assert: SimpleTestResource isAlreadyAvailable
				description: 'The resource was not set up again after the test']
]

{ #category : #private }
SimpleTestResourceTestCase >> dummy [
	self assert: resource hasSetup
		description: 'This test uses a resource but it was not set up'.
	self setRun.
	self assert: resource hasRun
		description: 'This test uses a resource but we could not interact with it'.
]

{ #category : #private }
SimpleTestResourceTestCase >> error [
	'foo' odd
]

{ #category : #private }
SimpleTestResourceTestCase >> setRun [
	resource setRun
]

{ #category : #running }
SimpleTestResourceTestCase >> setUp [
	"Ensure that we error, not just fail, if resource is nil so that #should:raise: checks cannot mistake such an error for what they are trapping."
	super setUp.
	resource := SimpleTestResource rawCurrentForTest.
	self
		deny: resource isNil
		description: 'SimpleTestResource has no current value in test'.
	self
		assert: resource class == SimpleTestResource
		description: 'SimpleTestResource current is not an instance of itself'.
	self
		assert: resource hasSetup
		description: 'This test uses a resource but it was not set up'
]

{ #category : #tests }
SimpleTestResourceTestCase >> testDebugTestWithResource [
	"The debug will raise an error if the resource is not set up properly."
	
	self clearOuterResourceStateDuring: [(self class selector: #setRun) debug]
]

{ #category : #tests }
SimpleTestResourceTestCase >> testResourceCollection [
	self assert: self class buildSuiteFromSelectors resources size = self resources size
		description: 'The suite should have the same number of resources as its test'.
	self class buildSuiteFromSelectors resources do:
		[:each |
		self assert: (self resources includes: each)
			description: each name , ':  I have this resource but my suite does not'].
]

{ #category : #tests }
SimpleTestResourceTestCase >> testResourceInitRelease [
	| suite error failure |
	suite := self classForTestSuite new.
	suite addTest: (error := self class selector: #error).
	suite addTest: (failure := self class selector: #fail).
	suite addTest: (self class selector: #dummy).
	suite run.
	self assert: resource hasSetup
]

{ #category : #tests }
SimpleTestResourceTestCase >> testRunSuiteWithResource [
	| suite |
	suite :=  self classForTestSuite new.
	suite addTest: (SimpleTestResourceTestCase selector: #error).
	suite addTest: (SimpleTestResourceTestCase selector: #fail).
	suite addTest: (self class selector: #dummy).
	self clearOuterResourceStateDuring:
		[self
			assert: suite run printString = '3 ran, 1 passed, 0 skipped, 0 expected failures, 1 failure, 1 error, 0 passed unexpected'
			description: 'A suite of tests needing SimpleTestResource did not run as expected'].
]

{ #category : #tests }
SimpleTestResourceTestCase >> testRunTestWithResource [
	self clearOuterResourceStateDuring:
		[self
			assert: (self class selector: #dummy) run printString =
							'1 ran, 1 passed, 0 skipped, 0 expected failures, 0 failures, 0 errors, 0 passed unexpected'
			description: 'A dummy test that needed a resource did not pass']
]
