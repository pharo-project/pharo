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
	#category : #'TestRunner-Core-Extensions'
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

{ #category : #running }
NewCommandLineRunner >> recordError: aTestFailure duringTest: aTestCase [
	
	aTestCase shouldPass ifFalse: [ ^ self ].
	self addError: aTestCase.

	self reportError: aTestFailure of: aTestCase
]

{ #category : #running }
NewCommandLineRunner >> recordFailure: aTestFailure duringTest: aTestCase [

	aTestCase shouldPass ifFalse: [ ^ self ].
	self addFailure: aTestCase.

	self reportFailure: aTestFailure of: aTestCase.
]

{ #category : #private }
NewCommandLineRunner >> recordPassOf: aTestCase [

	self result addPass: aTestCase.
	self reporter reportPass: aTestCase
]

{ #category : #internal }
NewCommandLineRunner >> recordSkip: aTestSkipped duringTest: aProcessMonitorTestServiceTest [

	self result addSkip: aTestSkipped
]

{ #category : #private }
NewCommandLineRunner >> recordUnexpectedPassOf: aTestCase [

	self notYetImplemented
]

{ #category : #reporting }
NewCommandLineRunner >> reportError: aTestError of: aTestCase [

	self reporter reportError: aTestError of: aTestCase
]

{ #category : #reporting }
NewCommandLineRunner >> reportFailure: aTestFailure of: aTestCase [

	self reporter reportFailure: aTestFailure of: aTestCase
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
		runBlock: [self runCaseManaged: aTestCase ]
]

{ #category : #running }
NewCommandLineRunner >> runCaseManaged: aTestCase [

	"saving the failure context"

	[ 
	self timeToRun: aTestCase.
	aTestCase shouldPass
		ifTrue: [ self recordPassOf: aTestCase ]
		ifFalse: [ self recordUnexpectedPassOf: aTestCase ] ]
		on: Exception
		do: [ :error | error recordResultOf: aTestCase inHDTestReport: self ]
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

{ #category : #running }
NewCommandLineRunner >> timeToRun: aTestCase [

	| initialMicroseconds finalMicroseconds duration |
	initialMicroseconds := Time microsecondClockValue.

	[ aTestCase runCaseManaged ]
		on: Exception
		do: [ :e | 
			finalMicroseconds := Time microsecondClockValue.
			duration := finalMicroseconds - initialMicroseconds.
			self reporter currentCaseDuration:
				(Duration milliSeconds: duration).
			e pass ].
	duration ifNil: [ 
		duration := Time microsecondClockValue - initialMicroseconds.
		self reporter currentCaseDuration: (Duration milliSeconds: duration) ]
]
