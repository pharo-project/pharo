"
I report tests execution result in the Transcript
"
Class {
	#name : #TranscriptReporter,
	#superclass : #AbstractReporter,
	#category : #'TestRunner-Core-Reporter'
}

{ #category : #reporting }
TranscriptReporter >> defaultTranscript [

	^ Transcript
]

{ #category : #reporting }
TranscriptReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	| testCaseExecutionDuration result |
	testCaseExecutionDuration := [ result := aBlock value ] timeToRun.
	self addTosuiteExecutionDuration: testCaseExecutionDuration.
	self defaultTranscript
		tab;
		show: aTestCase asString;
		show: ' duration: ';
		show: testCaseExecutionDuration;
		show: ' state: ';
		show: (result hasPassed
				 ifTrue: [ ' [passed]' ]
				 ifFalse: [ 
					 result hasFailures ifTrue: [ '[failure]' ].
					 result hasErrors ifTrue: [ '[errors]' ] ]);
		cr
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
