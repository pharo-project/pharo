"
I'm a command to perform a pull of incomming commits. 
I open a pull view window.
"
Class {
	#name : #IceTipPullCommand,
	#superclass : #IceTipRemoteActionCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipPullCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 120 
		for: IceTipRepositoryListContext
]

{ #category : #accessing }
IceTipPullCommand class >> defaultHelp [
	^ 'Pulls into image all commits from your version to the origin remote HEAD'
]

{ #category : #accessing }
IceTipPullCommand class >> defaultMenuIconName [
	^ #down
]

{ #category : #accessing }
IceTipPullCommand class >> defaultMenuItemName [
	^ 'Pull'
]

{ #category : #activation }
IceTipPullCommand class >> toolbarHistoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipHistoryContext
]

{ #category : #activation }
IceTipPullCommand class >> toolbarRepositoryActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipRepositoryContext
		
]

{ #category : #activation }
IceTipPullCommand class >> toolbarWorkingCopyActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation byRootGroupItemFor: IceTipWorkingCopyContext

]

{ #category : #execution }
IceTipPullCommand >> execute [
	self withErrorHandlingDo: [ 
		self validateRemotesPresent.
	 	(IceTipPullBrowser onRepositoryModel: self repositoryModel) openWithSpec ]

]

{ #category : #execution }
IceTipPullCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	(self repositoryModel isNotNil and: [self repositoryModel isMissing not]) ifTrue: [
		self badge: self repositoryModel numberOfIncomingCommits ]
]
