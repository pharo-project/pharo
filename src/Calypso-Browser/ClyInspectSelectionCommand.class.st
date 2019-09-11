"
I am a command to inspect current selected items in the browser.
If there is multiple selection I open inspector on selected list.
And when there single selection I open inspector directly on single item.

Internal Representation and Key Implementation Points.

    Instance Variables
	selectedItems:		<Collection of<Object>>
"
Class {
	#name : #ClyInspectSelectionCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'selectedItems'
	],
	#category : #'Calypso-Browser-Commands'
}

{ #category : #activation }
ClyInspectSelectionCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyBrowserContext
]

{ #category : #activation }
ClyInspectSelectionCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: PharoShortcuts current inspectItShortcut for: ClyBrowserContext
]

{ #category : #testing }
ClyInspectSelectionCommand class >> canBeExecutedInContext: aBrowserContext [
	^aBrowserContext hasSelectedItems
]

{ #category : #accessing }
ClyInspectSelectionCommand >> defaultMenuItemName [
	^'Inspect'
]

{ #category : #execution }
ClyInspectSelectionCommand >> execute [
	selectedItems size = 1 
		ifTrue: [ selectedItems first inspect ]
		ifFalse: [ selectedItems inspect]
]

{ #category : #execution }
ClyInspectSelectionCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	selectedItems := aBrowserContext selectedObjects
]
