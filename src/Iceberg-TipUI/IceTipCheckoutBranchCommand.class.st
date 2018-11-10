"
Command that opens a dialog to checkout a (new or already existing) branch.
"
Class {
	#name : #IceTipCheckoutBranchCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCheckoutBranchCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 200 
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipCheckoutBranchCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational
]

{ #category : #accessing }
IceTipCheckoutBranchCommand class >> defaultHelp [
	^ 'Checkout a new or existing branch.'
]

{ #category : #accessing }
IceTipCheckoutBranchCommand class >> defaultMenuIconName [
	^ #branch
]

{ #category : #accessing }
IceTipCheckoutBranchCommand class >> defaultMenuItemName [
	^ 'Checkout branch'
]

{ #category : #accessing }
IceTipCheckoutBranchCommand class >> defaultToolbarItemName [
	^ 'Branch'
]

{ #category : #activation }
IceTipCheckoutBranchCommand class >> toolbarHistoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
IceTipCheckoutBranchCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipRepositoryContext
		order: 1000
]

{ #category : #activation }
IceTipCheckoutBranchCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 1000
]

{ #category : #execution }
IceTipCheckoutBranchCommand >> execute [
	(IceTipCheckoutBranchDialog on: self repositoryModel) openDialogWithSpec
]
