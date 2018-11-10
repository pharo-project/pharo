"
Command to unload a package from the system, keeping it in the repository
"
Class {
	#name : #IceTipUnloadPackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipUnloadPackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 40 for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipUnloadPackageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext packageModel isLoaded
]

{ #category : #accessing }
IceTipUnloadPackageCommand class >> defaultHelp [
	^ 'Unload selected package from the system, but keep it in the repository'
]

{ #category : #accessing }
IceTipUnloadPackageCommand class >> defaultMenuIconName [
	^ #smallCancel
]

{ #category : #accessing }
IceTipUnloadPackageCommand class >> defaultMenuItemName [
	^ 'Unload'
]

{ #category : #execution }
IceTipUnloadPackageCommand >> execute [
	self packageModel unload
]
