"
Command that removes a package from the repository, without removing it from the system.
"
Class {
	#name : #IceTipRemoveFromRepositoryPackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipRemoveFromRepositoryPackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 110 for: IceTipWorkingCopyContext
]

{ #category : #accessing }
IceTipRemoveFromRepositoryPackageCommand class >> canBeExecutedInContext: aToolContext [
	^ true
]

{ #category : #accessing }
IceTipRemoveFromRepositoryPackageCommand class >> defaultHelp [
	^ 'Removes the current repository from the repository, without removing it from the system'
]

{ #category : #accessing }
IceTipRemoveFromRepositoryPackageCommand class >> defaultMenuIconName [
	^ #glamorousTrash
]

{ #category : #accessing }
IceTipRemoveFromRepositoryPackageCommand class >> defaultMenuItemName [
	^ 'Remove from repository'
]

{ #category : #execution }
IceTipRemoveFromRepositoryPackageCommand >> execute [
	self packageModel remove
]
