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
StdReporter >> reportSuite: aTestSuite runBlock: aBlock [

	"run an report the execution's result of a testSuite"

	maxTest := aTestSuite tests size.
	aBlock value
]
