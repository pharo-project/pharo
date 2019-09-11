"
I run all selected test cases
"
Class {
	#name : #ClyRunTestsFromClassesCommand,
	#superclass : #ClyRunTestsFromSelectedItemsCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyRunTestsFromClassesCommand class >> contextClass [
	^ClyClass asCalypsoItemContext 
]

{ #category : #execution }
ClyRunTestsFromClassesCommand >> execute [
	
	testItems do: [ :each | self runTestCase: each actualObject]
]

{ #category : #execution }
ClyRunTestsFromClassesCommand >> runTestCase: testCase [

	| testResult |
	testResult := testCase classForTestResult new.
	
	self runTestCase: testCase results: testResult.
	
	testResult updateResultsInHistory.
	
	self  
		notifyUserAboutResults: testResult 
		with: testCase name
]
