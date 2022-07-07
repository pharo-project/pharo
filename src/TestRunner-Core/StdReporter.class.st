"
I am a test result reporter in the console
"
Class {
	#name : #StdReporter,
	#superclass : #AbstractReporter,
	#instVars : [
		'stdout',
		'stderr',
		'maxTest',
		'currentTest'
	],
	#category : #'TestRunner-Core-Reporter'
}

{ #category : #accessing }
StdReporter >> currentTest [

	^ currentTest ifNil: [ currentTest := 0 ]
]

{ #category : #processing }
StdReporter >> incrementCurrentTest [

	currentTest := self currentTest + 1
]

{ #category : #accessing }
StdReporter >> maxTest [

	^ maxTest
]

{ #category : #reporting }
StdReporter >> printProgress [
	| string |
	string := '[',self currentTest asString, '/', self maxTest asString,']'.
	self stderr 
		startOfLine; right: (80 - string size); nextPutAll: string.
]

{ #category : #reporting }
StdReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	super reportCase: aTestCase runBlock: aBlock.
	self stderr
		startOfLine;
		clearToEnd;
		green;
		print: aTestCase;
		clear.
	self incrementCurrentTest.
	self printProgress
]

{ #category : #reporting }
StdReporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self stderr
		startOfLine;
		print:
			'Finishished running ' , aTestResult runCount asString , ' tests';
		startOfLine;
		print: aTestResult asString
]

{ #category : #reporting }
StdReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	maxTest := aTestSuite tests size.
	aBlock value
]

{ #category : #accessing }
StdReporter >> stderr [

	^ stderr ifNil: [ stderr := VTermOutputDriver stderr ]
]

{ #category : #accessing }
StdReporter >> stdout [

	^ stdout ifNil: [ stdout := VTermOutputDriver stdout ]
]
