"
I am a root of commands hierarchy which are supposed to run tests related to selected items.

I provide initialization, description and icon for subclasses.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	testItems:		<Collection of<ClyDataSourceItem>>
"
Class {
	#name : #ClyRunTestsFromSelectedItemsCommand,
	#superclass : #ClyTestCommand,
	#instVars : [
		'testItems'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyRunTestsFromSelectedItemsCommand class >> canBeExecutedInContext: aToolContext [

	^aToolContext selectedItems	anySatisfy: [ :each | 
			each hasProperty: ClyTestResultProperty ]
]

{ #category : #activation }
ClyRunTestsFromSelectedItemsCommand class >> contextClass [
	self subclassResponsibility 
]

{ #category : #activation }
ClyRunTestsFromSelectedItemsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: self contextClass
]

{ #category : #activation }
ClyRunTestsFromSelectedItemsCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $t meta for: self contextClass
]

{ #category : #activation }
ClyRunTestsFromSelectedItemsCommand class >> fullBrowserTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation priority: 1000 for: self contextClass
]

{ #category : #testing }
ClyRunTestsFromSelectedItemsCommand class >> isAbstract [
	^self = ClyRunTestsFromSelectedItemsCommand 
]

{ #category : #accessing }
ClyRunTestsFromSelectedItemsCommand >> defaultMenuItemName [ 
	^'Run tests'
]

{ #category : #execution }
ClyRunTestsFromSelectedItemsCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	testItems := aToolContext selectedItems 
		select: [ :each | each hasProperty: ClyTestResultProperty ]
]

{ #category : #execution }
ClyRunTestsFromSelectedItemsCommand >> runTestCase: testCase results: testResult [

	testCase resetHistory.
	testCase suite run: testResult
]

{ #category : #accessing }
ClyRunTestsFromSelectedItemsCommand >> testItems [
	^ testItems
]

{ #category : #accessing }
ClyRunTestsFromSelectedItemsCommand >> testItems: anObject [
	testItems := anObject
]

{ #category : #'context menu support' }
ClyRunTestsFromSelectedItemsCommand >> testResult [

	^self testResultOf: testItems first
]

{ #category : #'context menu support' }
ClyRunTestsFromSelectedItemsCommand >> testResultOf: anItem [

	^anItem getProperty: ClyTestResultProperty
]
