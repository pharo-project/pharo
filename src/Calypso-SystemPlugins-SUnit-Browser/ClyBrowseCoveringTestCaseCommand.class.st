"
I am a command that opens a new browser on the test class corresponding to the selected class.
"
Class {
	#name : 'ClyBrowseCoveringTestCaseCommand',
	#superclass : 'ClyBrowserCommand',
	#instVars : [
		'selectedClassItem'
	],
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyBrowseCoveringTestCaseCommand >> canBeExecuted [

	super canBeExecuted ifFalse: [ ^ false ].

	^ self context lastSelectedItem hasProperty: ClyTestedClassProperty
]

{ #category : 'execution' }
ClyBrowseCoveringTestCaseCommand >> executeRefactoring [

	| testCase |
	testCase := (selectedClassItem getProperty: ClyTestedClassProperty) coveringTestCase.
	self browser spawnBrowser: ClyFullBrowserMorph withState: [ :b | b selectClass: testCase ]
]

{ #category : 'testing' }
ClyBrowseCoveringTestCaseCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isAboutBrowserItemType: ClyClass
]

{ #category : 'accessing' }
ClyBrowseCoveringTestCaseCommand >> name [
	^ 'Browse test class'
]

{ #category : 'accessing' }
ClyBrowseCoveringTestCaseCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyBrowseCoveringTestCaseCommand >> prepareFullExecution [

	super prepareFullExecution.
	selectedClassItem := self context lastSelectedItem
]
