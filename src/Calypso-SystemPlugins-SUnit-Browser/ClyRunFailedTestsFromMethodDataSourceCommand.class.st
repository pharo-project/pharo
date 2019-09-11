"
I am a command to run all failed tests available in given data source
"
Class {
	#name : #ClyRunFailedTestsFromMethodDataSourceCommand,
	#superclass : #ClyRunTestsFromMethodDataSourceCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyRunFailedTestsFromMethodDataSourceCommand class >> canBeExecutedInContext: aBrowserContext [
	| testResult |
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false].
	
	testResult := aBrowserContext browser resultView dataSource getMetaProperty: ClyTestResultProperty.
	^testResult hasBrokenTests
]

{ #category : #activation }
ClyRunFailedTestsFromMethodDataSourceCommand class >> menuOrder [
	^super menuOrder + 1
]

{ #category : #execution }
ClyRunFailedTestsFromMethodDataSourceCommand >> buildTestSuite [
	
	| suite |
	suite := TestSuite new.
	
	self testMethodsDo: [ :eachMethod |
		eachMethod isFailedTest | eachMethod isErrorTest ifTrue: [ 
			suite addTest: (eachMethod methodClass selector: eachMethod selector)]].

	^suite
]

{ #category : #accessing }
ClyRunFailedTestsFromMethodDataSourceCommand >> defaultMenuItemName [
	self hasTestResult ifFalse: [ ^'Run failed' ].
	
	^'Run failed (', self testResult brokenCount asString, ')'
]
