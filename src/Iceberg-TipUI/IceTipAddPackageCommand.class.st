"
Command that opens a dialog to add a/many package/s into the selected repository.
"
Class {
	#name : #IceTipAddPackageCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipAddPackageCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $n meta for: IceTipWorkingCopyContext
]

{ #category : #accessing }
IceTipAddPackageCommand class >> defaultHelp [
	^ 'Adds an existing package to this repository'
]

{ #category : #accessing }
IceTipAddPackageCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipAddPackageCommand class >> defaultMenuItemName [
	^ 'Add package'
]

{ #category : #accessing }
IceTipAddPackageCommand class >> defaultPosition [
	^ #right
]

{ #category : #accessing }
IceTipAddPackageCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 20
]

{ #category : #execution }
IceTipAddPackageCommand >> execute [
	(IceTipAddPackagesDialog on: self repositoryModel) openDialogWithSpec 
]
