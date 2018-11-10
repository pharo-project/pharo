"
I checkout a specific commit from history.
I will let your repository in detach mode!
"
Class {
	#name : #IceTipCheckoutCommitCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'selectedCommit'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCheckoutCommitCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 10
		for: IceTipHistoryContext
]

{ #category : #accessing }
IceTipCheckoutCommitCommand class >> defaultHelp [
	^ 'Set the selected commit as current commit and load the code in the image'
]

{ #category : #activation }
IceTipCheckoutCommitCommand class >> defaultMenuIconName [
	^ #refresh
]

{ #category : #activation }
IceTipCheckoutCommitCommand class >> defaultMenuItemName [
	^ 'Checkout'
]

{ #category : #execution }
IceTipCheckoutCommitCommand >> execute [
	
	(IceTipCheckoutPreviewBrowser onBranch: selectedCommit) openWithSpec
]

{ #category : #execution }
IceTipCheckoutCommitCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	selectedCommit := aToolContext selectedCommit
]
