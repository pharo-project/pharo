"
I'm a command to perform a push of outgoing commits. 
I open a push view window.
"
Class {
	#name : #IceTipPushCommand,
	#superclass : #IceTipRemoteActionCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipPushCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 130 
		for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipPushCommand class >> defaultHelp [
	^ 'Pushes pending commits to the origin repository (in current branch)'
]

{ #category : #accessing }
IceTipPushCommand class >> defaultMenuIconName [
	^ #up
]

{ #category : #accessing }
IceTipPushCommand class >> defaultMenuItemName [
	^ 'Push'
]

{ #category : #activation }
IceTipPushCommand class >> toolbarHistoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
IceTipPushCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipRepositoryContext
		
]

{ #category : #activation }
IceTipPushCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipWorkingCopyContext

]

{ #category : #execution }
IceTipPushCommand >> execute [
	self withErrorHandlingDo: [ 
		self validateRemotesPresent.
		(IceTipPushBrowser onRepositoryModel: self repositoryModel) openWithSpec ]
]

{ #category : #execution }
IceTipPushCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	(self repositoryModel isNotNil and: [self repositoryModel isMissing not]) ifTrue: [  
		self badge: self repositoryModel numberOfOutgoingCommits ]
]
