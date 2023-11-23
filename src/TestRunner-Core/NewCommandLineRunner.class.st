"
I allow to run tests from commandLine and export it according to the giving Reporter: 
- XmlReporter 
- StdReporter
- TranscriptReporter 
I extend NewTestRunner so i share it public API. In addition:

## Public API
- reporter:
"
Class {
	#name : 'NewCommandLineRunner',
	#superclass : 'NewTestRunner',
	#instVars : [
		'reporter'
	],
	#category : 'TestRunner-Core-Extensions',
	#package : 'TestRunner-Core',
	#tag : 'Extensions'
}

{ #category : 'instance creation' }
NewCommandLineRunner class >> with: aReporterClass [

	^ self new reporter: aReporterClass new
]

{ #category : 'accessing' }
NewCommandLineRunner >> classForReporter [

	^ AbstractReporter
]

{ #category : 'private' }
NewCommandLineRunner >> classForTestFailure [

	^ TestFailure
]

{ #category : 'test' }
NewCommandLineRunner >> hasDefaultReporter [
	"check if the Runner has a default reporter"

	^ self reporter isNotNil
]

{ #category : 'initialization' }
NewCommandLineRunner >> initialize [
	super initialize.
	self reporter: self classForReporter stdReporter.
]

{ #category : 'recording' }
NewCommandLineRunner >> recordError: aTestFailure duringTest: aTestCase [
	"store the test case then report it as error"

	aTestCase shouldPass ifFalse: [ ^ self ].
	self addError: aTestCase.
	self reportError: aTestFailure of: aTestCase
]

{ #category : 'recording' }
NewCommandLineRunner >> recordFailure: aTestFailure duringTest: aTestCase [
	"store the test case then report it as failure"

	"aTestCase shouldPass ifFalse: [ ^ self ]."
	self addFailure: aTestCase.
	self reportFailure: aTestFailure of: aTestCase
]

{ #category : 'private' }
NewCommandLineRunner >> recordPassOf: aTestCase [
	"store the test case then report it as passed test"

	self result addPass: aTestCase.
	self reporter reportPass: aTestCase
]

{ #category : 'internal' }
NewCommandLineRunner >> recordSkip: aTestSkipped duringTest: aTestCase [
	"store the test case then report it as skipped test"

	self result addSkip: aTestCase.
	self reporter reportSkipped: aTestCase

]

{ #category : 'private' }
NewCommandLineRunner >> recordUnexpectedPassOf: aTestCase [
	"store the testcase as failure. A test which is excpected to fail should not pass"

	| exception |
	exception := self classForTestFailure new messageText:
		             'Unexpected pass (test is marked as expected failure)'.
	self recordFailure: exception duringTest: aTestCase
]

{ #category : 'reporting' }
NewCommandLineRunner >> reportError: aTestError of: aTestCase [
	"store the test case then report it as error"

	self reporter reportError: aTestError of: aTestCase
]

{ #category : 'reporting' }
NewCommandLineRunner >> reportFailure: aTestFailure of: aTestCase [
	"store the test case then report it as failure"

	self reporter reportFailure: aTestFailure of: aTestCase

]

{ #category : 'private' }
NewCommandLineRunner >> reporter [

	^ reporter
]

{ #category : 'accessing' }
NewCommandLineRunner >> reporter: aReporter [

	reporter := aReporter
]

{ #category : 'processing' }
NewCommandLineRunner >> resetReporter [

	self reporter: self reporter class new
]

{ #category : 'running' }
NewCommandLineRunner >> runCase: aTestCase [
	"run and report a testCase"

	self reporter
		reportCase: aTestCase
		runBlock: [self runCaseManaged: aTestCase ]
]

{ #category : 'running' }
NewCommandLineRunner >> runCaseManaged: aTestCase [
	"run then report the test according to its state (passed, error, failure) during the execution"

	[ 
	self timeToRun: aTestCase.
	aTestCase shouldPass
		ifTrue: [ self recordPassOf: aTestCase ]
		ifFalse: [ self recordUnexpectedPassOf: aTestCase ] ]
		on: Exception
		do: [ :error | error recordResultOf: aTestCase inHDTestReport: self ]
]

{ #category : 'running' }
NewCommandLineRunner >> runSuite [
	"report a testsuite while running it"
	
	self reporter 
	 reportSuite: self suite 
	 runBlock: [ super runSuite ]
]

{ #category : 'processing' }
NewCommandLineRunner >> tearDown [

	super tearDown.
	self reporter reportResult: self result.
	self resetReporter.
]

{ #category : 'private' }
NewCommandLineRunner >> timeToRun: aTestCase [
	"run a testcase and compute the duration"
	
	| initialMicroseconds duration |
	initialMicroseconds := Time microsecondClockValue.

	[ aTestCase runCaseManaged ]
		on: Exception
		do: [ :e | 
			duration := Time microsecondClockValue - initialMicroseconds.
			self reporter currentCaseDuration:
				(Duration milliSeconds: duration).
			e pass ].
	duration ifNil: [ 
		duration := Time microsecondClockValue - initialMicroseconds.
		self reporter currentCaseDuration: (Duration milliSeconds: duration) ]

]
