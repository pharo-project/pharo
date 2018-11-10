"
Command that opens a dialog to select a branch to merge in the current merge
"
Class {
	#name : #IceTipMergeCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipMergeCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 200 
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipMergeCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational
]

{ #category : #accessing }
IceTipMergeCommand class >> defaultHelp [
	^ 'Merge your current branch with another.'
]

{ #category : #accessing }
IceTipMergeCommand class >> defaultMenuIconName [
	^ #merge
]

{ #category : #accessing }
IceTipMergeCommand class >> defaultMenuItemName [
	^ 'Merge branch'
]

{ #category : #accessing }
IceTipMergeCommand class >> defaultToolbarItemName [
	^ 'Merge'
]

{ #category : #activation }
IceTipMergeCommand class >> toolbarHistoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
IceTipMergeCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipRepositoryContext
		order: 1010
]

{ #category : #activation }
IceTipMergeCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 1010
]

{ #category : #execution }
IceTipMergeCommand >> execute [
	(IceTipMergeBranchDialog on: self repositoryModel) openDialogWithSpec
]
