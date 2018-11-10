"
I an a commander command that will be activated on any commitish or repository and ask the selected item to create a tag on itself.
"
Class {
	#name : #IceTipCreateTagCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'selectedCommitish'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCreateTagCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 200 
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipCreateTagCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 10
		for: IceTipHistoryContext
]

{ #category : #accessing }
IceTipCreateTagCommand class >> defaultHelp [
	^ 'Creates a tag for the selected commit'
]

{ #category : #activation }
IceTipCreateTagCommand class >> defaultMenuIconName [
	^ #glamorousBookmark
]

{ #category : #activation }
IceTipCreateTagCommand class >> defaultMenuItemName [
	^ 'Create tag'
]

{ #category : #execution }
IceTipCreateTagCommand >> execute [
	
	(IceTipCreateTagPanel on: selectedCommitish) openDialogWithSpec
]

{ #category : #execution }
IceTipCreateTagCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	selectedCommitish := aToolContext item
]
