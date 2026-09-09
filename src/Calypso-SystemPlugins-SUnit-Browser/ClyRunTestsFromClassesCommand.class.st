"
I run all selected test cases
"
Class {
	#name : 'ClyRunTestsFromClassesCommand',
	#superclass : 'ClyRunTestsFromSelectedItemsCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyRunTestsFromClassesCommand class >> contextClass [
	^ClyClass asCalypsoItemContext
]

{ #category : 'accessing' }
ClyRunTestsFromClassesCommand >> description [

	^ 'Run all selected test cases'
]

{ #category : 'execution' }
ClyRunTestsFromClassesCommand >> execute [

	testItems do: [ :each | self runTestCase: each actualObject]
]

{ #category : 'execution' }
ClyRunTestsFromClassesCommand >> runTestCase: testCase [

	| testResult |
	testResult := testCase classForTestResult new.

	self runTestCase: testCase results: testResult.

	testResult updateResultsInHistory.

	self
		notifyUserAboutResults: testResult
		with: testCase name
]
