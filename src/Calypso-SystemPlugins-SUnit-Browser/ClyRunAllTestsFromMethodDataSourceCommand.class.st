"
I am a command to run all tests available in given data source
"
Class {
	#name : #ClyRunAllTestsFromMethodDataSourceCommand,
	#superclass : #ClyRunTestsFromMethodDataSourceCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #execution }
ClyRunAllTestsFromMethodDataSourceCommand >> buildTestSuite [
	
	| suite |
	suite := TestSuite new.
	
	self testMethodsDo: [ :eachMethod |
			suite addTest: (eachMethod methodClass selector: eachMethod selector)].

	^suite
]

{ #category : #accessing }
ClyRunAllTestsFromMethodDataSourceCommand >> defaultMenuItemName [
	self hasTestResult ifFalse: [ ^'Run all' ].
	
	^'Run all (', self testResult allCount asString, ')'
]
