"
I will allow the ""forget"" of a repository (remote it from list of registered repositories).
I also add the possibility of remove the repository from file system.
"
Class {
	#name : #IceTipForgetRepositoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipForgetRepositoryCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 10000 "put it at the end"
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipForgetRepositoryCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $x meta for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipForgetRepositoryCommand class >> confirmMessage [
	^ 'Are you sure to remove repository: {name}?
(Packages will not be deleted, nor the repository from the file system)'
]

{ #category : #accessing }
IceTipForgetRepositoryCommand class >> defaultHelp [
	^ 'Removes this repository from the list of managed repositories (it will not remove any local repository)'
]

{ #category : #accessing }
IceTipForgetRepositoryCommand class >> defaultMenuIconName [
	^ #remove
]

{ #category : #accessing }
IceTipForgetRepositoryCommand class >> defaultMenuItemName [
	^ 'Forget repository'
]

{ #category : #execution }
IceTipForgetRepositoryCommand >> execute [
	(IceTipRemoveRepositoryDialog on: self repositoryModel) openDialogWithSpec
]
