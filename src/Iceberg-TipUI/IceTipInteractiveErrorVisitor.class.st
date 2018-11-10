"
I'm a visitor to provide users with an interactive way to react to iceberg errors.

"
Class {
	#name : #IceTipInteractiveErrorVisitor,
	#superclass : #IceErrorVisitor,
	#instVars : [
		'repository'
	],
	#category : #'Iceberg-TipUI'
}

{ #category : #'instance creation' }
IceTipInteractiveErrorVisitor class >> on: aModel [
	^ self new repository: aModel
]

{ #category : #accessing }
IceTipInteractiveErrorVisitor >> repository [
	^ repository
]

{ #category : #accessing }
IceTipInteractiveErrorVisitor >> repository: anObject [
	repository := anObject
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitAuthenticationError: anError [
	UIManager default 
		alert: anError messageText
		title: 'Iceberg authentication Error'
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitCheckoutConflict: anError [
	UIManager default 
		alert: anError messageText
		title: 'Iceberg conflicts while checking out'
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitCloneLocationAlreadyExists: anError [ 
	UIManager default 
		alert: ('The clone location {1} already exists' format: { anError location })
		title: 'Clone action failed'	
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitCloneRemoteNotFound: anError [
	UIManager default 
		alert: ('The clone remote {1} could not been found' format: { anError remoteUrl })
		title: 'Clone action failed'	
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitGenericError: anError [
	
	(IceTipErrorDialog on: anError) openDialogWithSpec
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitMergeAborted: anError [
	UIManager default 
		alert: (anError messageText ifNil: ['Automatic (fast-forward) merge is not possible. 
Please perform the merge manually, then proceed with your previous operation.
WARNING: If you have external files, you may need to verify the conflicts there.'])
		title: 'Automatic merge failed'
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitMissingCredentialsError: anError [

	((IceTipGitUsernameAndEmailDialog on: self repository)
		onAccept: [ anError resume ];
		openDialogWithSpec)
			modalRelativeTo: World.
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitNoCommitMessage: aWarning [
	| proceed |
	
	proceed := UIManager default
		confirm: aWarning messageText
		label: 'Warning!'  
		trueChoice: 'Commit' 
		falseChoice: 'Cancel'
		cancelChoice: nil
		default: false.
	
	proceed ifNil: [ ^ self ].
	proceed ifTrue: [ aWarning resume ]
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitNoRemote: anError [
	UIManager default 
		alert: 'There are no remotes defined for this repository. 
Add one before continue.'
		title: 'Remote action failed'	
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitNothingToCommitError: anError [
	UIManager default inform: 'There is nothing to commit.'
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitRemoteAlreadyExistError: anError [ 
	UIManager default inform: ('Remote "{1}" already exists.' format: { anError remoteName })
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitRemoteDesynchronizedError: anError [
	| continue command |
	
	continue := UIManager default
		confirm: ('Your repository is out of sync with remote {1}. 
You need to pull remote changes before continue and push your changes.' format: { anError remote })
		label: 'Remote repository out of sync!'
		trueChoice: 'Pull'
		falseChoice: 'Cancel'
		cancelChoice: nil
		default: false.	
	(continue isNil or: [continue not])
		ifTrue: [ ^ self ].	

	command := IcePullRemoteCommand new
		repository: self repository;
		remote: anError remote;
		yourself.

	[ command execute ]
	on: IceMergeAborted, IceShouldCommitBeforePull
	do: [ :e |
		self flag: #todo. "Refactor this" 
		(e isKindOf: IceShouldCommitBeforePull)
			ifTrue: [ e resume ]
			ifFalse: [ e acceptError: self ] ].
	
	command isSuccess ifFalse: [ ^ self ].
	anError isResumable ifTrue: [ ^ anError resume ].
	anError retry
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitShouldCommitBeforePullError: anError [
	| continue |
	continue := UIManager default
		confirm:
			'Your repository has uncommited changes. Merging incoming commits will change
your current working copy and your current state will not be recoverable. 
We recommend that you commit first and then pull incoming changes again.'
		label: 'You might loose your current changes!'
		trueChoice: 'Proceed anyway'
		falseChoice: 'Cancel'
		cancelChoice: nil
		default: false.	
	continue ifFalse: [ ^ self ].
	anError resume
]

{ #category : #visiting }
IceTipInteractiveErrorVisitor >> visitWorkingCopyDesynchronizedError: anError [
	| solveStrategy command |
	solveStrategy := UIManager default 
		chooseFrom: #(
			'Create new branch and commit' 
			'Merge local copy into image and commit')		
		values: { 
			IceCreateBranchCommand. 
			IceMergeWorkingCopyCommand }
		message: 'Your loaded code is not synchronized with the local sources in your repository. 
You need to solve this problem before you continue, please choose one of the strategies below.'
		title: 'Loaded code out of sync!'.
	solveStrategy ifNil: [ ^ self ].
	(command := solveStrategy new) 
		repository: self repository;
		execute.
	command isSuccess 
		ifTrue: [ anError retry ]
]
