"
I am an unborn branch. I exist only on a new repository that has no commits inside. I manage the special case of a first repository commit and branch creation.
"
Class {
	#name : #IceUnbornBranch,
	#superclass : #IceCommitish,
	#category : #'Iceberg-Core'
}

{ #category : #comparing }
IceUnbornBranch >> = anotherBranch [

	^ anotherBranch isUnbornBranch
]

{ #category : #'API-changes' }
IceUnbornBranch >> changesFromCommit: anIceNoCommit [ 
	"I am an unborn branch. This means the repository is not yet created and the commit is a no-commit"
	
	self assert: anIceNoCommit isNoCommit.
	^ #()
]

{ #category : #accessing }
IceUnbornBranch >> checkout: aCheckoutStreategy [

	"Do nothing. An unborn branch is already HEAD"
]

{ #category : #accessing }
IceUnbornBranch >> commit [
	
	^ IceNoCommit repository: repository
]

{ #category : #printing }
IceUnbornBranch >> description [
	
	^ self name
]

{ #category : #testing }
IceUnbornBranch >> hasIncomingCommits [
	
	^ self hasIncomingCommitsFrom: self repository origin
]

{ #category : #'API-remotes' }
IceUnbornBranch >> hasIncomingCommitsFrom: aRemote [
	
	aRemote fetch.
	^ aRemote hasRemoteBranchNamed: self name
]

{ #category : #testing }
IceUnbornBranch >> hasOutgoingCommits [
	
	^ false
]

{ #category : #'API-remotes' }
IceUnbornBranch >> hasOutgoingCommitsTo: aRemote [

	^ false
]

{ #category : #testing }
IceUnbornBranch >> hasUpstream [
	^ false
]

{ #category : #'API-remotes' }
IceUnbornBranch >> incomingCommits [
	"I am unborn. I have no commits"
	^ #()
]

{ #category : #'API-remotes' }
IceUnbornBranch >> incomingCommitsFrom: aRemote [
	| remoteBranch |
	remoteBranch := aRemote remoteBranchNamed: self name ifAbsent: [ ^ #() ].
	^ remoteBranch newCommitWalk
		uptoCommit: self commit;
		commits
]

{ #category : #testing }
IceUnbornBranch >> isBranch [
	
	^ true
]

{ #category : #testing }
IceUnbornBranch >> isHead [
	
	^ true
]

{ #category : #testing }
IceUnbornBranch >> isLocal [
	
	^ true
]

{ #category : #testing }
IceUnbornBranch >> isRemote [
	
	^ false
]

{ #category : #testing }
IceUnbornBranch >> isUnbornBranch [
	
	^ true
]

{ #category : #accessing }
IceUnbornBranch >> name [

	self subclassResponsibility
]

{ #category : #'API-history' }
IceUnbornBranch >> newCommitWalk [
	
	^ IceNullCommitWalk new
]

{ #category : #'API-remotes' }
IceUnbornBranch >> outgoingCommits [
	"I am unborn. I have no commits"
	^ #()
]

{ #category : #'API-packages' }
IceUnbornBranch >> packageNames [
	
	^ #()
]

{ #category : #'API-packages' }
IceUnbornBranch >> packages [

	^ #()
]

{ #category : #'API-remotes' }
IceUnbornBranch >> pull [
	
	self pullFrom: repository origin
]

{ #category : #'API-remotes' }
IceUnbornBranch >> pullFrom: aRemote [
	
	"This is a special operation: this branch is unborn.
	This means there is no local branch in ths repository.
	We shall then
	  - create a local branch tracking a remote branch with the same name
	  - pull from that branch
	
	If there is no branch in the remote repository with the same name, do nothing.
	"
	
	| remoteBranch |
	repository fetch.
	remoteBranch := aRemote remoteBranchNamed: self name ifAbsent: [ ^ self ].
	repository createBranch: self name inCommit: remoteBranch commit.
]

{ #category : #'API-remotes' }
IceUnbornBranch >> push [
	
	"Do nothing"
]

{ #category : #'API-remotes' }
IceUnbornBranch >> pushTo: aRemote [

	"Do Nothing"
]

{ #category : #'private-monticello' }
IceUnbornBranch >> snapshotFor: anIceSavedPackage [ 
	
	"I am an unborn branch, I have no code, so my snapshot is empty"
	^ MCSnapshot empty
]

{ #category : #'API-remotes' }
IceUnbornBranch >> tracksRemoteBranch [
	
	^ false
]
