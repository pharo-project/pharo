"
Command that opens the repair window on a repository
"
Class {
	#name : #IceTipRepairCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipRepairCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 1
		for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipRepairCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational not
]

{ #category : #accessing }
IceTipRepairCommand class >> defaultHelp [
	^ 'Helps you to repair your repository when it is detached.'
]

{ #category : #accessing }
IceTipRepairCommand class >> defaultMenuIconName [
	^ #repair
]

{ #category : #accessing }
IceTipRepairCommand class >> defaultMenuItemName [
	^ 'Repair repository'
]

{ #category : #accessing }
IceTipRepairCommand class >> defaultToolbarItemName [
	^ 'Repair'
]

{ #category : #activation }
IceTipRepairCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 1
]

{ #category : #execution }
IceTipRepairCommand >> execute [
	(IceTipRepairRepositoryDialog on: self repositoryModel) openDialogWithSpec
]
