"
I allow to run tests from commandLine
"
Class {
	#name : #NewCommandLineRunner,
	#superclass : #NewTestRunner,
	#instVars : [
		'testCount',
		'reporter'
	],
	#category : #'TestRunner-Core-Core'
}

{ #category : #private }
NewCommandLineRunner >> defaultReporterClass [

	^ TranscriptReporter
]

{ #category : #test }
NewCommandLineRunner >> hasDefaultReporter [

	^ self reporter isNotNil
]

{ #category : #private }
NewCommandLineRunner >> incrementTestCount [

	testCount ifNil: [ testCount := 0 ].
	testCount := testCount + 1
]

{ #category : #private }
NewCommandLineRunner >> recordPassOf: aTestCase [

	self result addPass: aTestCase
]

{ #category : #private }
NewCommandLineRunner >> recordUnexpectedPassOf: aTestCase [

	self notYetImplemented
]

{ #category : #private }
NewCommandLineRunner >> reporter [

	^ reporter ifNil: [ reporter := self defaultReporterClass new ]
]

{ #category : #private }
NewCommandLineRunner >> reporter: aReporter [

	reporter := aReporter
]

{ #category : #running }
NewCommandLineRunner >> runCase: aTestCase [

	"run and report a testCase"

	self incrementTestCount.
	self reporter 
	 reportCase: aTestCase 
	 runBlock: [ 
		aTestCase runCaseManaged.
		aTestCase shouldPass
			ifTrue: [ self recordPassOf: aTestCase ]
			ifFalse: [ self recordUnexpectedPassOf: aTestCase ] ]
]

{ #category : #running }
NewCommandLineRunner >> runSuite [

	self reporter reportSuite: self suite runBlock: [ super runSuite ]
]

{ #category : #running }
NewCommandLineRunner >> tearDown [

	super tearDown.
	self reporter reportResult: self result
]

{ #category : #private }
NewCommandLineRunner >> testCount [

	"return the number of executed testCases"

	^ testCount
]
