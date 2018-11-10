"
Command that browses the selected package as it is loaded in the image.
"
Class {
	#name : #IceTipBrowsePackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipBrowsePackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 10 for: IceTipWorkingCopyContext
]

{ #category : #activation }
IceTipBrowsePackageCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $b meta for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipBrowsePackageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext packageModel isLoaded
]

{ #category : #accessing }
IceTipBrowsePackageCommand class >> defaultHelp [
	^ 'Browse the selected package as it is loaded in the image'
]

{ #category : #accessing }
IceTipBrowsePackageCommand class >> defaultMenuItemName [
	^ 'Browse'
]

{ #category : #execution }
IceTipBrowsePackageCommand >> execute [
	self packageModel browsePackage
]
