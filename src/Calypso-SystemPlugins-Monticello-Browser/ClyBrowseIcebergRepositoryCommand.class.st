Class {
	#name : #ClyBrowseIcebergRepositoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #activation }
ClyBrowseIcebergRepositoryCommand class >> browserContextActivation [
	<classAnnotation>
	
	"The Default option when double clicking should be the first in the menu"
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 1.1
		for: IceTipRepositoryListContext
]

{ #category : #activation }
ClyBrowseIcebergRepositoryCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $b meta for: IceTipRepositoryListContext
]

{ #category : #accessing }
ClyBrowseIcebergRepositoryCommand class >> defaultHelp [
	^ 'Browse the version in repository'
]

{ #category : #accessing }
ClyBrowseIcebergRepositoryCommand class >> defaultMenuIconName [
	^ #glamorousBrowse
]

{ #category : #accessing }
ClyBrowseIcebergRepositoryCommand class >> defaultMenuItemName [
	^ 'Browse repository version'
]

{ #category : #execution }
ClyBrowseIcebergRepositoryCommand >> execute [
	| env commit snapshot |
	commit := repositoryModel commit.
	env := RGEnvironment new.
	commit packageNames 
		do: [ :packageName |
			snapshot := commit snapshotFor: (RPackage named: packageName).
			snapshot importInto: env asPackageNamed: packageName ] 
		displayingProgress: 'loading'.
	env clean.
	env browse.
]
