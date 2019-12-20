"
I am a command to open Calypso browser on given package from given Iceberg repository
"
Class {
	#name : #ClyBrowseIcebergPackageCommand,
	#superclass : #IceTipPackageCommand,
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #activation }
ClyBrowseIcebergPackageCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemOrder: 20 for: IceTipWorkingCopyContext
]

{ #category : #activation }
ClyBrowseIcebergPackageCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $b meta for: IceTipWorkingCopyContext
]

{ #category : #activation }
ClyBrowseIcebergPackageCommand class >> browserStrongSelectionActivation [
	<classAnnotation>
	
	^ IceTipCommandStrongSelectionActivation for: IceTipWorkingCopyContext
]

{ #category : #accessing }
ClyBrowseIcebergPackageCommand class >> defaultHelp [
	^ 'Browse the version in the repository'
]

{ #category : #accessing }
ClyBrowseIcebergPackageCommand class >> defaultMenuIconName [
	^ #glamorousBrowse
]

{ #category : #accessing }
ClyBrowseIcebergPackageCommand class >> defaultMenuItemName [
	^ 'Browse repository version'
]

{ #category : #execution }
ClyBrowseIcebergPackageCommand >> execute [
	| env commit snapshot |
	env := RGEnvironment new.
	commit := repositoryModel commit.
	
	#(1) do: [ :i | 
			snapshot := commit snapshotFor: (RPackage named: packageModel name).
 			snapshot importInto: env asPackageNamed: packageModel name]
		displayingProgress: 'loading'.
	env clean.
	env browseAs: repositoryModel entity description
]
