"
I open a the commit window, with all latest changes.
"
Class {
	#name : #IceTipCommitCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipCommitCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 100
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipCommitCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipCommitCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryOperational
]

{ #category : #accessing }
IceTipCommitCommand class >> defaultHelp [
	^ 'Commit changes to this repository'
]

{ #category : #accessing }
IceTipCommitCommand class >> defaultMenuIconName [
	^ #smallOk
]

{ #category : #accessing }
IceTipCommitCommand class >> defaultMenuItemName [
	^ 'Commit'
]

{ #category : #activation }
IceTipCommitCommand class >> packageListShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $s meta for: IceTipWorkingCopyContext
]

{ #category : #activation }
IceTipCommitCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipWorkingCopyContext
		order: 10
]

{ #category : #execution }
IceTipCommitCommand >> execute [
	(IceTipCommitBrowser on: self repositoryModel) openWithSpec
]
