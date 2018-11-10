"
I'm a command to open the package manager browser for a project.
"
Class {
	#name : #IceTipManagePackagesCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipManagePackagesCommand class >> browserContextActivation [
	<classAnnotation>
	
	"The Default option when double clicking should be the first in the menu"
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 1
		for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipManagePackagesCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $p meta for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipManagePackagesCommand class >> browserStrongSelectionActivation [
	<classAnnotation>
	
	^ IceTipCommandStrongSelectionActivation for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipManagePackagesCommand class >> defaultHelp [
	^ 'Open packages manager browser for this repository'
]

{ #category : #accessing }
IceTipManagePackagesCommand class >> defaultMenuIconName [
	^ #package
]

{ #category : #accessing }
IceTipManagePackagesCommand class >> defaultMenuItemName [
	^ 'Packages'
]

{ #category : #execution }
IceTipManagePackagesCommand >> execute [
	(IceTipWorkingCopyBrowser on: self repositoryModel) openWithSpec
]
