"
I register Iceberg projects.
I will open a dialog to take the new possible repository (from available options) and I will register it into the IceRepository>>#registry list.
"
Class {
	#name : #IceTipAddRepositoryCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipAddRepositoryCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $n meta for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipAddRepositoryCommand class >> defaultHelp [
	^ 'Add a repository (new, local, cloned) to your managed repositories.'
]

{ #category : #accessing }
IceTipAddRepositoryCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipAddRepositoryCommand class >> defaultMenuItemName [
	^ 'Add'
]

{ #category : #accessing }
IceTipAddRepositoryCommand class >> defaultPosition [
	^ #right
]

{ #category : #activation }
IceTipAddRepositoryCommand class >> toolbarRepositoryListActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipRepositoryListContext
		order: 100
]

{ #category : #execution }
IceTipAddRepositoryCommand >> execute [
	IceTipRegisterRepositoryDialog new openDialogWithSpec
]
