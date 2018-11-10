"
Action to start the creation of a new branch from the selected commit.
Then checkout the selected branch.
"
Class {
	#name : #IceTipCheckoutNewBranchCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'selectedCommit'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCheckoutNewBranchCommand class >> browserHistoryContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
		byRootGroupItemOrder: 10
		for: IceTipHistoryContext
]

{ #category : #accessing }
IceTipCheckoutNewBranchCommand class >> defaultHelp [
	^ 'Create a new branch from the selected commit and switch to it'
]

{ #category : #activation }
IceTipCheckoutNewBranchCommand class >> defaultMenuIconName [
	^ #branch
]

{ #category : #activation }
IceTipCheckoutNewBranchCommand class >> defaultMenuItemName [
	^ 'Branch...'
]

{ #category : #execution }
IceTipCheckoutNewBranchCommand >> execute [

	(IceTipCheckoutNewBranchPanel on: self repositoryModel)
		commitish: selectedCommit entity;
		withResultDo: [ :branch | branch checkout ];
		openDialogWithSpec
]

{ #category : #execution }
IceTipCheckoutNewBranchCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	selectedCommit := aToolContext item
]
