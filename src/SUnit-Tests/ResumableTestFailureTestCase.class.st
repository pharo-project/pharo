"
SUnit tests for ResumableTestFailure
"
Class {
	#name : #ResumableTestFailureTestCase,
	#superclass : #TestCase,
	#instVars : [
		'duplicateFailureLog'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #history }
ResumableTestFailureTestCase class >> lastStoredRun [
	^ ((Dictionary new) add: (#passed->((Set new) add: #testResumable; yourself)); add: (#timeStamp->'22 November 2008 10:11:35 pm'); add: (#failures->((Set new))); add: (#errors->((Set new))); yourself)
]

{ #category : #'test data' }
ResumableTestFailureTestCase >> errorTest [
	1 zork
]

{ #category : #'test data' }
ResumableTestFailureTestCase >> failureTest [
	self
		assert: false description: 'You should see me' resumable: true; 
		assert: false description: 'You should see me too' resumable: true; 
		assert: false description: 'You should see me last' resumable: false; 
		assert: false description: 'You should not see me' resumable: true
]

{ #category : #logging }
ResumableTestFailureTestCase >> logFailure: aString [
	duplicateFailureLog add: aString.
	super logFailure: aString.
]

{ #category : #'test data' }
ResumableTestFailureTestCase >> okTest [
	self assert: true
]

{ #category : #'test data' }
ResumableTestFailureTestCase >> regularTestFailureTest [
	self assert: false description: 'You should see me'
]

{ #category : #'Not categorized' }
ResumableTestFailureTestCase >> resumableTestFailureTest [
	self
		assert: false description: 'You should see more than me' resumable: true; 
		assert: false description: 'You should see more than me' resumable: true; 
		assert: false description: 'You should see me last' resumable: false; 
		assert: false description: 'You should not see me' resumable: true
]

{ #category : #running }
ResumableTestFailureTestCase >> setUp [
	super setUp.
	duplicateFailureLog := OrderedCollection with: 'In set up'.
]

{ #category : #running }
ResumableTestFailureTestCase >> tearDown [
	self deny: 'You should not see me' = duplicateFailureLog last
			description: 'We saw the ''You should not see me'' failure'.
	self deny: 'You should see more than me' = duplicateFailureLog last
			description: 'We did not see more than a ''You should see more than me'' failure'.
	super tearDown 
]

{ #category : #running }
ResumableTestFailureTestCase >> testResumable [
	| result suite |
	suite := self classForTestSuite new.
	suite addTest: (self class selector: #errorTest).
	suite addTest: (self class selector: #regularTestFailureTest).
	suite addTest: (self class selector: #resumableTestFailureTest).
	suite addTest: (self class selector: #okTest).
	result := suite run.
	self
		assert: result failures size = 2;
		assert: result errors size = 1
]
