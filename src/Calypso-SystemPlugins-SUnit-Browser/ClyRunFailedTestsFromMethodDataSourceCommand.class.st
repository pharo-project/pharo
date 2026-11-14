"
I am a command to run all failed tests available in given data source
"
Class {
	#name : 'ClyRunFailedTestsFromMethodDataSourceCommand',
	#superclass : 'ClyRunTestsFromMethodDataSourceCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'execution' }
ClyRunFailedTestsFromMethodDataSourceCommand >> buildTestSuite [

	| suite |
	suite := TestSuite new.

	self testMethodsDo: [ :eachMethod |
		eachMethod isFailedTest | eachMethod isErrorTest ifTrue: [
			suite addTest: (eachMethod methodClass selector: eachMethod selector)]].

	^suite
]

{ #category : 'testing' }
ClyRunFailedTestsFromMethodDataSourceCommand >> canBeExecuted [

	| testResult |
	super canBeExecuted ifFalse: [ ^ false ].

	testResult := self context browser resultView dataSource getMetaProperty: ClyTestResultProperty.
	^ testResult hasBrokenTests
]

{ #category : 'accessing' }
ClyRunFailedTestsFromMethodDataSourceCommand >> name [
	self hasTestResult ifFalse: [ ^'Run failed' ].

	^'Run failed (', self testResult brokenCount asString, ')'
]
