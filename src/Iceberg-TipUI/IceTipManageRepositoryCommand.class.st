"
I'm a command to show the repository manager for a repository model. 

"
Class {
	#name : #IceTipManageRepositoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipManageRepositoryCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryMissing not
]

{ #category : #accessing }
IceTipManageRepositoryCommand class >> defaultHelp [
	^ 'Open repository manager browser for this repository.'
]

{ #category : #accessing }
IceTipManageRepositoryCommand class >> defaultMenuIconName [
	^ #komitterSmalltalkhubRemote
]

{ #category : #accessing }
IceTipManageRepositoryCommand class >> defaultMenuItemName [
	^ 'Repository'
]

{ #category : #accessing }
IceTipManageRepositoryCommand class >> defaultPosition [
	^ #right
]

{ #category : #activation }
IceTipManageRepositoryCommand class >> packageListShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $r meta for: IceTipWorkingCopyContext
]

{ #category : #accessing }
IceTipManageRepositoryCommand class >> repositoryListContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 210
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipManageRepositoryCommand class >> repositoryListShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $r meta for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipManageRepositoryCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 10
]

{ #category : #execution }
IceTipManageRepositoryCommand >> execute [
	(IceTipRepositoryBrowser on: self repositoryModel) openWithSpec
]
