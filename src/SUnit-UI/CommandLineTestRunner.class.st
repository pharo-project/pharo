"
I am command line test runner. I run a TestSuite and outpout the progress in a terminal friendly way.
"
Class {
	#name : #CommandLineTestRunner,
	#superclass : #HDTestReport,
	#instVars : [
		'stderr',
		'stdout',
		'currentTest',
		'maxTest',
		'shouldSerializeError'
	],
	#category : #'SUnit-UI-Headless'
}

{ #category : #private }
CommandLineTestRunner >> createStderr [
	^ Stdio stderr
]

{ #category : #private }
CommandLineTestRunner >> createStdout [
	^ Stdio stdout
]

{ #category : #running }
CommandLineTestRunner >> done [
	
]

{ #category : #helper }
CommandLineTestRunner >> handleFailure: anError of: aTestCase [ 
	(anError isNil or: [aTestCase isExpectedFailure])  ifTrue: [ ^ self ].

	(anError isKindOf: TestFailure)
		ifTrue: [ 
			suiteFailures := suiteFailures + 1.
			self printFailure: anError of: aTestCase ]
		ifFalse: [ 
			suiteErrors := suiteErrors + 1.
			self printError: anError of: aTestCase ].
	
	self shouldSerializeError 
		ifTrue: [ self serializeError: anError of: aTestCase ]
]

{ #category : #helper }
CommandLineTestRunner >> handleSkip: aTestSkip of: aTestCase [ 
	self printSkipOf: aTestCase

]

{ #category : #helper }
CommandLineTestRunner >> increaseTestCount [
	currentTest := currentTest + 1.
]

{ #category : #initialization }
CommandLineTestRunner >> initialize [
	super initialize.
	shouldSerializeError := false.
]

{ #category : #helper }
CommandLineTestRunner >> print: longErrorMessage short: shortErrorMessage of: aTestCase [ 
	| cr |

	cr := OSPlatform current lineEnding.
	self stdout 
		<< (shortErrorMessage trimBoth padRightTo: 10)
		<< aTestCase printString
		<< cr.
	longErrorMessage isEmptyOrNil 
		ifFalse: [ self stdout << longErrorMessage << cr ]
]

{ #category : #helper }
CommandLineTestRunner >> printError: anError of: aTestCase [ 
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [ERROR]' 
		of: aTestCase 
]

{ #category : #helper }
CommandLineTestRunner >> printFailure: anError of: aTestCase [ 
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [FAIL]' 
		of: aTestCase 
]

{ #category : #helper }
CommandLineTestRunner >> printPassOf: aTestCase [
	self
		print: nil
		short: ' [OK]' 
		of: aTestCase 
]

{ #category : #printing }
CommandLineTestRunner >> printReport [
	| cr |
	cr := OSPlatform current lineEnding.
	
	self stderr
		<< cr
		<< 'Finished running ' << maxTest asString  
		<< ' Test' << (maxTest = 1 ifTrue: [''] ifFalse: ['s'])
		<< cr
]

{ #category : #helper }
CommandLineTestRunner >> printSkipOf: aTestCase [
	self 
		print: nil
		short: ' [SKIPPED]' 
		of: aTestCase
]

{ #category : #printing }
CommandLineTestRunner >> printTestCase: aTestCase [
]

{ #category : #running }
CommandLineTestRunner >> runCase: aTestCase [
	self increaseTestCount.
	self printTestCase: aTestCase.
	
	[[ aTestCase runCaseManaged. 
		self printPassOf: aTestCase ] 
		on: Halt , Error, TestFailure
		do: [ :err | self handleFailure: err of: aTestCase ]]
		on: TestSkip do: [ :skip| self handleSkip: skip of: aTestCase ]
]

{ #category : #running }
CommandLineTestRunner >> setUp [
	
	currentTest := 0.
	maxTest := suite tests size.
]

{ #category : #helper }
CommandLineTestRunner >> shortStackDescriptionFor: anError [
	^ (String streamContents: [ :s| 
			s << anError printString; cr.
			anError signalerContext shortDebugStackOn: s ])
		copyReplaceAll: String cr
		with: OSPlatform current lineEnding
]

{ #category : #accessing }
CommandLineTestRunner >> shouldSerializeError [
	^ shouldSerializeError
]

{ #category : #accessing }
CommandLineTestRunner >> shouldSerializeError: aBoolean [
	shouldSerializeError := aBoolean.
]

{ #category : #accessing }
CommandLineTestRunner >> stderr [
	(stderr isNil or: [ stderr closed ])
		ifTrue: [ stderr := self createStderr ].
	^ stderr 
]

{ #category : #accessing }
CommandLineTestRunner >> stdout [
	(stdout isNil or: [ stdout closed ])
		ifTrue: [ stdout := self createStdout ].
		
	^ stdout
]

{ #category : #running }
CommandLineTestRunner >> tearDown [
	self printReport
]
