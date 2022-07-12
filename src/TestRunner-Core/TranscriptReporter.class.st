"
I report tests execution result in the Transcript
"
Class {
	#name : #TranscriptReporter,
	#superclass : #AbstractReporter,
	#category : #'TestRunner-Core-Reporters'
}

{ #category : #reporting }
TranscriptReporter >> defaultTranscript [

	^ Transcript
]

{ #category : #reporting }
TranscriptReporter >> partialReport: aTestCase [

	self defaultTranscript
		tab;
		show: aTestCase asString;
		show: ' duration: ';
		show: currentCaseDuration;
		show: ' state:'
]

{ #category : #reporting }
TranscriptReporter >> reportError: aTestError of: aTestCase [

	self partialReport: aTestCase.
	self defaultTranscript
		show: ' [error]';
		cr
]

{ #category : #reporting }
TranscriptReporter >> reportFailure: aTestFailure of: aTestCase [

	self partialReport: aTestCase.
	self defaultTranscript
		show: ' [failure]';
		cr
]

{ #category : #reporting }
TranscriptReporter >> reportPass: aTestCase [

	"report the execution's result of a passed test"

	self partialReport: aTestCase.
	self defaultTranscript
		show: ' [passed]';
		lf
]

{ #category : #reporting }
TranscriptReporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self defaultTranscript
		show: 'result: ';
		show: aTestResult
]

{ #category : #reporting }
TranscriptReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	aBlock value.
	self defaultTranscript
		cr;
		show: 'duration: ';
		show: self suiteExecutionDuration;
		cr
]

{ #category : #reporting }
TranscriptReporter >> suiteExecutionDuration [

	^ suiteExecutionDuration ifNil: [ ^ Duration new ]
]
