"
I report test execution result with XTerm characters
"
Class {
	#name : #XTermReporter,
	#superclass : #StdReporter,
	#category : #'TestRunner-Core-Reporters'
}

{ #category : #accessing }
XTermReporter >> defaultOutputDriver [

	^ Stdio
]

{ #category : #reporting }
XTermReporter >> printProgress [

	| string cr |
	cr := OSPlatform current lineEnding.
	string := '        [',self currentTest asString, '/', self maxTest asString,']'.
	self stderr 
	 << string 
	 << cr
]

{ #category : #reporting }
XTermReporter >> report: aTestCase description: aDescription state: aString [

	
	self stdout 
	 << (aString, ' ')
	 << aTestCase printString
]

{ #category : #reporting }
XTermReporter >> reportError: aTestError of: aTestCase [

	self
		report: aTestCase
		description: aTestError messageText
		state: '[error]'
]

{ #category : #reporting }
XTermReporter >> reportFailure: aTestFailure of: aTestCase [

	self
		report: aTestCase
		description: aTestFailure messageText
		state: '[Failure]'.
	self printProgress
]

{ #category : #reporting }
XTermReporter >> reportPass: aTestCase [

	self
		report: aTestCase
		description: nil
		state: '[ok]'.
	self printProgress.
]

{ #category : #reporting }
XTermReporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self stderr << ('Finishished running ' , aTestResult runCount asString
	, ' tests.')
]

{ #category : #reporting }
XTermReporter >> reportSkipped: aTestCase [

	self
		report: aTestCase
		description: nil
		state: '[skipped]'.
	self printProgress.
]
