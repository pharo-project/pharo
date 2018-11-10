"
Command that reloads the code of an already loaded package, discarding all changes done in the meantime that were not committed.
"
Class {
	#name : #IceTipReloadPackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipReloadPackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 30 for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipReloadPackageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext packageModel isLoaded
]

{ #category : #accessing }
IceTipReloadPackageCommand class >> defaultHelp [
	^ 'Reload selected package, discarding all changes'
]

{ #category : #accessing }
IceTipReloadPackageCommand class >> defaultMenuIconName [
	^ #refresh
]

{ #category : #accessing }
IceTipReloadPackageCommand class >> defaultMenuItemName [
	^ 'Reload'
]

{ #category : #execution }
IceTipReloadPackageCommand >> execute [
	self packageModel reload.
	Iceberg announcer announce: (IceRepositoryModified for: self repositoryModel entity)
]
