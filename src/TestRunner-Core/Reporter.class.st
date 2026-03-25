"
I am an abstract class which specify the behaviour for reporting the result of tests execution 
"
Class {
	#name : 'Reporter',
	#superclass : 'Object',
	#instVars : [
		'suiteExecutionDuration',
		'currentCaseDuration'
	],
	#category : 'TestRunner-Core-Reporters',
	#package : 'TestRunner-Core',
	#tag : 'Reporters'
}

{ #category : 'testing' }
Reporter class >> isAbstract [

	^ self = Reporter
]

{ #category : 'factory' }
Reporter class >> stdReporter [
	^ StdReporter new
]

{ #category : 'factory' }
Reporter class >> xmlReporter [
	^ XmlReporter new
]

{ #category : 'internal' }
Reporter >> addToSuiteExecutionDuration: aDuration [

	suiteExecutionDuration := self suiteExecutionDuration + aDuration
]

{ #category : 'reporting' }
Reporter >> currentCaseDuration: aDuration [

	currentCaseDuration := aDuration
]

{ #category : 'reporting' }
Reporter >> datasAreReseted [

	^ suiteExecutionDuration isNil and: currentCaseDuration isNil
]

{ #category : 'reporting' }
Reporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	aBlock value.

	self updateSuiteExecutionDuration
]

{ #category : 'reporting' }
Reporter >> reportError: aTestError of: aTestCase [

	self subclassResponsibility
]

{ #category : 'reporting' }
Reporter >> reportFailure: aTestFailure of: aTestCase [

	self subclassResponsibility
]

{ #category : 'reporting' }
Reporter >> reportPass: aTestCase [

	"report the execution's result of a passed test"

	self subclassResponsibility
]

{ #category : 'reporting' }
Reporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self subclassResponsibility
]

{ #category : 'reporting' }
Reporter >> reportSkipped: aTestCase [
	"report the giving testcase"
	
	
]

{ #category : 'reporting' }
Reporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	self subclassResponsibility
]

{ #category : 'accessing' }
Reporter >> suiteExecutionDuration [

	^ suiteExecutionDuration ifNil: [ 
		  suiteExecutionDuration := Duration new ]
]

{ #category : 'reporting' }
Reporter >> updateSuiteExecutionDuration [

	self addToSuiteExecutionDuration: currentCaseDuration
]
