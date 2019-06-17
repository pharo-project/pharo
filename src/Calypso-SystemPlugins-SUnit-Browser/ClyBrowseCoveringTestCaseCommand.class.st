"
I am a command that opens a new browser on the test class corresponding to the selected class.
"
Class {
	#name : #ClyBrowseCoveringTestCaseCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'selectedClassItem'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyBrowseCoveringTestCaseCommand class >> canBeExecutedInContext: aBrowserContext [
	(super canBeExecutedInContext: aBrowserContext) ifFalse: [ ^false ].
	
	^ aBrowserContext lastSelectedItem hasProperty: ClyTestedClassProperty
]

{ #category : #activation }
ClyBrowseCoveringTestCaseCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyQueryMenuGroup
		for: ClyClass asCalypsoItemContext 
]

{ #category : #accessing }
ClyBrowseCoveringTestCaseCommand >> defaultMenuItemName [
	^ 'Browse test class'
]

{ #category : #execution }
ClyBrowseCoveringTestCaseCommand >> execute [
	| testCase |
	testCase := (selectedClassItem getProperty: ClyTestedClassProperty) coveringTestCase.
	
	browser spawnBrowser: ClyFullBrowser withState: [ :b |
		b selectClass: testCase]
]

{ #category : #execution }
ClyBrowseCoveringTestCaseCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	selectedClassItem := aToolContext lastSelectedItem
]
