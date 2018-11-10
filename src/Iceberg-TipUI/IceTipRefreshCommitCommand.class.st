"
Command that launches a refresh of the commit tree during commit
"
Class {
	#name : #IceTipRefreshCommitCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'tool',
		'announcer'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRefreshCommitCommand class >> defaultHelp [
	^ 'Refresh this change tree'
]

{ #category : #accessing }
IceTipRefreshCommitCommand class >> defaultMenuIconName [
	^ #glamorousRefresh
]

{ #category : #accessing }
IceTipRefreshCommitCommand class >> defaultMenuItemName [
	^ 'Refresh'
]

{ #category : #accessing }
IceTipRefreshCommitCommand class >> defaultPosition [
	^ #right
]

{ #category : #activation }
IceTipRefreshCommitCommand class >> toolbarCommitActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipCommitContext
]

{ #category : #acccessing }
IceTipRefreshCommitCommand >> announcer [
	^ announcer ifNotNil: #contents
]

{ #category : #execution }
IceTipRefreshCommitCommand >> execute [
	self repositoryModel reset.
	"Announce to my owner (not the general announcement, because just my owner panel is 
	 interested on this action)"
	self announcer announce: (IceTipDiffRefreshed for: self repositoryModel entity)
]

{ #category : #execution }
IceTipRefreshCommitCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	announcer := IceTipWeakValueHolder contents: aToolContext toolAnnouncer
	
]
