"
Command that opens a dialog to add a remote to a repository.
"
Class {
	#name : #IceTipAddRemoteCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipAddRemoteCommand class >> defaultHelp [
	^ 'Adds a remote to this repository'
]

{ #category : #accessing }
IceTipAddRemoteCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipAddRemoteCommand class >> defaultMenuItemName [
	^ 'Add remote'
]

{ #category : #accessing }
IceTipAddRemoteCommand class >> defaultPosition [
	^ #right
]

{ #category : #accessing }
IceTipAddRemoteCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipRepositoryContext
]

{ #category : #execution }
IceTipAddRemoteCommand >> execute [
	(IceTipAddRemoteDialog on: self repositoryModel) openDialogWithSpec
]
