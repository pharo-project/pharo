Class {
	#name : #TestExecutionServiceTestCase,
	#superclass : #TestCase,
	#instVars : [
		'testService',
		'testEnvironment'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #testing }
TestExecutionServiceTestCase class >> isAbstract [ 
	^self = TestExecutionServiceTestCase 
]

{ #category : #running }
TestExecutionServiceTestCase >> createTestService [

	self subclassResponsibility 
]

{ #category : #running }
TestExecutionServiceTestCase >> runCaseManaged [
	"Here we are testing the test environment logic.
	So we should disable it for ourselves"
	
	^DefaultExecutionEnvironment beActiveDuring: [ self runCase]
]

{ #category : #running }
TestExecutionServiceTestCase >> setUp [
	super setUp.
	
	testEnvironment := TestExecutionEnvironment new.
	testService := self createTestService

]
