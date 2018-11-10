"
Command that opens a browser on the code of the repository.

The current implementation relies on a monticello MCSnapshotBrowser
"
Class {
	#name : #IceTipBrowseRepositoryPackageVersionCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipBrowseRepositoryPackageVersionCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 20 for: IceTipWorkingCopyContext
]

{ #category : #accessing }
IceTipBrowseRepositoryPackageVersionCommand class >> defaultHelp [
	^ 'Browse the version in the repository'
]

{ #category : #accessing }
IceTipBrowseRepositoryPackageVersionCommand class >> defaultMenuItemName [
	^ 'Browse repository version'
]

{ #category : #execution }
IceTipBrowseRepositoryPackageVersionCommand >> execute [
	self packageModel browseRepositoryVersion
]
