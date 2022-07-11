"
I am an abstract class which specify the behaviour for reporting the result of tests execution 
"
Class {
	#name : #AbstractReporter,
	#superclass : #Object,
	#instVars : [
		'suiteExecutionDuration',
		'currentCaseDuration'
	],
	#category : #'TestRunner-Core-Reporter'
}

{ #category : #testing }
AbstractReporter class >> isAbstract [

	^ self = AbstractReporter
]

{ #category : #internal }
AbstractReporter >> addToSuiteExecutionDuration: aDuration [

	suiteExecutionDuration := self suiteExecutionDuration + aDuration
]

{ #category : #reporting }
AbstractReporter >> currentCaseDuration: aDuration [

	currentCaseDuration := aDuration
]

{ #category : #reporting }
AbstractReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	aBlock value.

	self updateSuiteExecutionDuration
]

{ #category : #reporting }
AbstractReporter >> reportError: aTestError of: aTestCase [

	self subclassResponsibility
]

{ #category : #reporting }
AbstractReporter >> reportFailure: aTestFailure of: aTestCase [

	self subclassResponsibility
]

{ #category : #reporting }
AbstractReporter >> reportPass: aTestCase [

	"report the execution's result of a passed test"

	self subclassResponsibility
]

{ #category : #reporting }
AbstractReporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self subclassResponsibility
]

{ #category : #reporting }
AbstractReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	self subclassResponsibility
]

{ #category : #accessing }
AbstractReporter >> suiteExecutionDuration [

	^ suiteExecutionDuration ifNil: [ 
		  suiteExecutionDuration := Duration new ]
]

{ #category : #reporting }
AbstractReporter >> updateSuiteExecutionDuration [

	self addToSuiteExecutionDuration: currentCaseDuration
]
