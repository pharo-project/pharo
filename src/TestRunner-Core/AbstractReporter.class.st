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
AbstractReporter >> addTosuiteExecutionDuration: aDuration [

	suiteExecutionDuration := self suiteExecutionDuration + aDuration
]

{ #category : #reporting }
AbstractReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	currentCaseDuration := aBlock timeToRun
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

	^ self suiteExecutionDuration ifNil: [ 
		  suiteExecutionDuration := Duration new ]
]
