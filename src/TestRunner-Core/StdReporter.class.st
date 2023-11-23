"
I am a test result reporter in the console
"
Class {
	#name : 'StdReporter',
	#superclass : 'AbstractReporter',
	#instVars : [
		'stdout',
		'stderr',
		'maxTest',
		'currentTest'
	],
	#category : 'TestRunner-Core-Reporters',
	#package : 'TestRunner-Core',
	#tag : 'Reporters'
}

{ #category : 'accessing' }
StdReporter >> currentTest [

	^ currentTest ifNil: [ currentTest := 1 ]
]

{ #category : 'processing' }
StdReporter >> incrementCurrentTest [

	currentTest := self currentTest + 1
]

{ #category : 'accessing' }
StdReporter >> maxTest [

	^ maxTest
]

{ #category : 'reporting' }
StdReporter >> printProgress [
	| string |
	string := '[',self currentTest asString, '/', self maxTest asString,']'.
	self stderr 
		startOfLine; right: (80 - string size); nextPutAll: string.
]

{ #category : 'reporting' }
StdReporter >> reportCase: aTestCase runBlock: aBlock [

	"run an report the execution's result of a testCase"

	super reportCase: aTestCase runBlock: aBlock.
	self incrementCurrentTest.
]

{ #category : 'reporting' }
StdReporter >> reportError: aTestError of: aTestCase [

	self stderr
		startOfLine;
		clearToEnd;
		red;
		print: aTestCase;
		nextPutAll: '     X error';
		clear.
	self printProgress.
]

{ #category : 'reporting' }
StdReporter >> reportFailure: aTestFailure of: aTestCase [

	self stderr
		startOfLine;
		clearToEnd;
		yellow;
		print: aTestCase;
		nextPutAll: '     /!\ failure';
		clear.
	self printProgress.
]

{ #category : 'reporting' }
StdReporter >> reportPass: aTestCase [

	"report the execution's result of a passed test"

	self stderr
		startOfLine;
		clearToEnd;
		green;
		print: aTestCase;
		clear.
	self printProgress
]

{ #category : 'reporting' }
StdReporter >> reportResult: aTestResult [

	"report the execution's result of a testSuite"

	self stderr
		startOfLine;
		clearToEnd;
		print:
			'Finishished running ' , aTestResult runCount asString , ' tests';
		startOfLine;
		clearToEnd;
		print: aTestResult asString
]

{ #category : 'reporting' }
StdReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	maxTest := aTestSuite tests size.
	aBlock value
]

{ #category : 'accessing' }
StdReporter >> stderr [

	^ stderr ifNil: [ stderr := VTermOutputDriver stderr ]
]

{ #category : 'accessing' }
StdReporter >> stdout [

	^ stdout ifNil: [ stdout := VTermOutputDriver stdout ]
]
