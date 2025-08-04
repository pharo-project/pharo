"
I run all selected test methods using selected test cases (runTestCases) as their implementation.
I am allow browser to run inherited test method by selected class.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	runTestCases:		<Collection of<TestCase class>>
"
Class {
	#name : 'ClyRunTestsFromMethodsCommand',
	#superclass : 'ClyRunTestsFromSelectedItemsCommand',
	#instVars : [
		'runTestCases'
	],
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyRunTestsFromMethodsCommand class >> contextClass [
	^ClyMethod asCalypsoItemContext
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> defaultMenuIconName [

	| fullResult |
	fullResult := self testResult.
	runTestCases do: [ :each |
		fullResult concreteResultOf: runTestCases first ifPresent: [:testResult |
			^testResult iconName ]].

	^fullResult iconName
]

{ #category : 'accessing' }
ClyRunTestsFromMethodsCommand >> description [

	^ 'Runs all selected test methods using selected test cases (runTestCases)'
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> execute [
	testItems do: [:each | self runTestItem: each ]
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.

	runTestCases := aToolContext selectedClasses
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> runTest: testSelector of: testClass [

	| testResult |
	testResult := testClass debug: testSelector.

	self
		notifyUserAboutResults: testResult
		with: 'Method: ' , testClass asString , '>>#' , testSelector asString
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> runTestItem: testItem [

	| testSelector |
	testSelector := self testSelectorOf: testItem.

	(self targetTestCasesFor: testItem) do: [ :each |
		self runTest: testSelector of: each ]
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> targetTestCasesFor: testItem [
	| fullResult itemTestCases |
	itemTestCases := IdentitySet new.
	fullResult := self testResultOf: testItem.

	runTestCases do: [:each |
		fullResult concreteResultOf: each ifPresent: [ :concreteResult |
			itemTestCases add: each ]].

	^itemTestCases ifEmpty: [fullResult testCases ]
]

{ #category : 'execution' }
ClyRunTestsFromMethodsCommand >> testSelectorOf: testMethodItem [

	^testMethodItem name asSymbol
]
