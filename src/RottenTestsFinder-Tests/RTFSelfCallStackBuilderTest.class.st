"
A RTFSelfCallInterpreterTest is a test class for testing the behavior of RTFSelfCallInterpreter
"
Class {
	#name : 'RTFSelfCallStackBuilderTest',
	#superclass : 'RTFMetaTest',
	#instVars : [
		'stackBuilder'
	],
	#category : 'RottenTestsFinder-Tests-SelfInterpreter',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'SelfInterpreter'
}

{ #category : 'accessing' }
RTFSelfCallStackBuilderTest >> fixtureClass [ 
	
	^ RTFFakeTestClassTest
]

{ #category : 'running' }
RTFSelfCallStackBuilderTest >> setUp [
	"See RTFFakeTestSuperClass comment for more information."
	super setUp.
	stackBuilder := RTFSelfCallStackBuilder new 
		considerClassesThat: [ :class | class inheritsFrom: TestAsserter ]; yourself
]

{ #category : 'accessing' }
RTFSelfCallStackBuilderTest >> stackBuilder [

	^ stackBuilder
]

{ #category : 'tests' }
RTFSelfCallStackBuilderTest >> testCallingSuperHelper [

	self stackBuilder send: #testCallingSuperHelper fromClass: self fixtureClass.

	self
		assertCollection: self stackBuilder selfSentMethods hasSameElements: { RTFFakeTestSuperClassTest >> #superAssertHelper }
]

{ #category : 'tests' }
RTFSelfCallStackBuilderTest >> testSimple [
	
	self stackBuilder send: #testFailingButNotConsideredAsRotten fromClass: self fixtureClass.

	self
		assertEmpty: self stackBuilder selfSentMethods
]

{ #category : 'tests' }
RTFSelfCallStackBuilderTest >> testWithHelper [
	
	self stackBuilder send: #testWithAssertionInHelperRun fromClass: self fixtureClass.

	self
		assertCollection: self stackBuilder selfSentMethods hasSameElements: { self fixtureClass >> #assertHelper }
]

{ #category : 'tests' }
RTFSelfCallStackBuilderTest >> testWithHelperHelper [

	self stackBuilder send: #testWithHelperHelper fromClass: self fixtureClass.
	self assertCollection: self stackBuilder selfSentMethods hasSameElements: {
			(self fixtureClass >> #helperHelper).
			(self fixtureClass >> #assertHelper) }
]

{ #category : 'tests' }
RTFSelfCallStackBuilderTest >> testWithSuperHelper [
	
	self stackBuilder send: #testWithAssertionInSuperHelperRun fromClass: self fixtureClass.

	self
		assertCollection: self stackBuilder selfSentMethods hasSameElements: { RTFFakeTestSuperClassTest >> #superAssertHelper }
]
