"
I'm a command to perform a fetch operation on selected project.
"
Class {
	#name : #IceTipFetchCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipFetchCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 140 
		for: IceTipRepositoryListContext
]

{ #category : #activation }
IceTipFetchCommand class >> browserShortcutActivation [
	"<classAnnotation>"
	
	^ CmdShortcutActivation by: $s meta for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipFetchCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isRepositoryMissing not 
		and: [ aToolContext repositoryModel hasRemotes ]
]

{ #category : #accessing }
IceTipFetchCommand class >> defaultHelp [
	^ 'Fetch (bring new commits) from this repositories.'
]

{ #category : #accessing }
IceTipFetchCommand class >> defaultMenuIconName [
	^ #smallUpdate
]

{ #category : #accessing }
IceTipFetchCommand class >> defaultMenuItemName [
	^ 'Fetch'
]

{ #category : #activation }
IceTipFetchCommand class >> toolbarHistoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
IceTipFetchCommand class >> toolbarPullActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipPullContext
]

{ #category : #activation }
IceTipFetchCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipRepositoryContext

]

{ #category : #activation }
IceTipFetchCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipWorkingCopyContext

]

{ #category : #execution }
IceTipFetchCommand >> execute [
	self repositoryModel fetch
]
