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
		'maxTest'
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
CommandLineTestRunner >> increaseTestCount [
	currentTest := currentTest + 1
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

{ #category : #printing }
CommandLineTestRunner >> printTestCase: aTestCase [
]

{ #category : #helper }
CommandLineTestRunner >> recordPassOf: aTestCase [
	self
		print: nil
		short: ' [OK]'
		of: aTestCase
]

{ #category : #helper }
CommandLineTestRunner >> recordSkip: aTestSkip duringTest: aTestCase [
	self
		print: nil
		short: ' [SKIPPED]'
		of: aTestCase
]

{ #category : #running }
CommandLineTestRunner >> reportTestCase: aTestCase runBlock: aBlock [

	self increaseTestCount.
	self printTestCase: aTestCase.
	aBlock value
]

{ #category : #running }
CommandLineTestRunner >> setUp [

	currentTest := 0.
	maxTest := suite tests size
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

{ #category : #helper }
CommandLineTestRunner >> writeError: anError of: aTestCase [
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [ERROR]'
		of: aTestCase
]

{ #category : #helper }
CommandLineTestRunner >> writeFailure: anError of: aTestCase [
	self
		print: (self shortStackDescriptionFor: anError)
		short: ' [FAIL]'
		of: aTestCase
]
