"
Command that opens a dialog to remove a package.
It gives the user with two checkboxes to choose:
 - remove from repository
 - unload

Right now this is only shown for loaded packages.
"
Class {
	#name : #IceTipUnloadAndRemovePackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipUnloadAndRemovePackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 100 for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipUnloadAndRemovePackageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext packageModel isLoaded
]

{ #category : #accessing }
IceTipUnloadAndRemovePackageCommand class >> defaultHelp [
	^ 'Opens a dialog to choose between unloading from system and/or removing a package from the repository'
]

{ #category : #accessing }
IceTipUnloadAndRemovePackageCommand class >> defaultMenuIconName [
	^ #glamorousTrash
]

{ #category : #accessing }
IceTipUnloadAndRemovePackageCommand class >> defaultMenuItemName [
	^ 'Unload and/or remove from repository'
]

{ #category : #execution }
IceTipUnloadAndRemovePackageCommand >> execute [

	(IceTipRemovePackageDialog on: self packageModel) openDialogWithSpec
]
