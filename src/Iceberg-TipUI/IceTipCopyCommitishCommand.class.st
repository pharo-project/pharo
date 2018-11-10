"
Command that copies the selected entity short commit ID to the clipboard
"
Class {
	#name : #IceTipCopyCommitishCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCopyCommitishCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 20
		for: IceTipHistoryContext
]

{ #category : #activation }
IceTipCopyCommitishCommand class >> browserRepositoryListActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byItemOf: CmdExtraMenuGroup
		order: 10000
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipCopyCommitishCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext item commitId isNotNil
]

{ #category : #accessing }
IceTipCopyCommitishCommand class >> defaultHelp [
	^ 'Copy the selected short commit ID (7 characters long) to the clipboard'
]

{ #category : #accessing }
IceTipCopyCommitishCommand class >> defaultMenuIconName [
	^ #book
]

{ #category : #accessing }
IceTipCopyCommitishCommand class >> defaultMenuItemName [
	^ 'Copy commitish ID'
]

{ #category : #execution }
IceTipCopyCommitishCommand >> execute [
	item copyCommitIDToClipboard.
]

{ #category : #execution }
IceTipCopyCommitishCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item
]
