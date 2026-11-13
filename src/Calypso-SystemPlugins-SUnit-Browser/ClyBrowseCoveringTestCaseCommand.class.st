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
ClyBrowseCoveringTestCaseCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].

	^ aBrowserContext lastSelectedItem hasProperty: ClyTestedClassProperty
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
ClyBrowseCoveringTestCaseCommand >> menuGroup [

	^ ClyQueryMenuGroup
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
