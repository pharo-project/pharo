"
Command that loads the current non-loaded selected package
"
Class {
	#name : #IceTipLoadPackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipLoadPackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 40 for: IceTipWorkingCopyContext
]

{ #category : #testing }
IceTipLoadPackageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext packageModel isLoaded not
]

{ #category : #accessing }
IceTipLoadPackageCommand class >> defaultHelp [
	^ 'Load selected package in the image'
]

{ #category : #accessing }
IceTipLoadPackageCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipLoadPackageCommand class >> defaultMenuItemName [
	^ 'Load'
]

{ #category : #execution }
IceTipLoadPackageCommand >> execute [
	self packageModel load.
	Iceberg announcer announce: (IceRepositoryModified for: self repositoryModel entity)
]
