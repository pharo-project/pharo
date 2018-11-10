"
I am a local branch. My main difference with remote branches is that I may have a remote tracked branch. This remote tracked branch allows me to calculate my incoming and outgoing commits.

  branch incomingCommits.
  branch hasIncomingCommits.
  branch outgoingCommits.
  branch hasOutgoingCommits.

I can moreover calculate incoming and outgoing commits to/from a given remote.

  branch incomingCommitsFrom: aRemote.
  branch hasIncomingCommitsFrom: aRemote.
  branch outgoingCommitsTo: aRemote.
  branch hasOutgoingCommitsTo: aRemote.

Notice that none of these operations fetches the remote, thus the information may not be up to date. You may explicitly fetch before doing these questions.

A local branch can also pull from its tracked branch or pull from a given remote.

  branch pull.
  branch pullFrom: aRemote.
"
Class {
	#name : #IceLocalBranch,
	#superclass : #IceBranch,
	#category : #'Iceberg-Core'
}

{ #category : #comparing }
IceLocalBranch >> = aBranch [

	^ aBranch isLocal
		and: [ self name = aBranch name ]
]

{ #category : #accessing }
IceLocalBranch >> commit: anIceCommit [
	
	self subclassResponsibility
]

{ #category : #testing }
IceLocalBranch >> hasIncomingCommits [
	
	self hasUpstream ifFalse: [ ^ false ].
	^ self hasIncomingCommitsFrom: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> hasIncomingCommitsFrom: aRemote [
	
	| localCommits remoteCommits |
	self tracksRemoteBranch
		ifFalse: [ ^ false ].
	
	localCommits := self commits.
	remoteCommits := self trackedBranch commits.
	^ remoteCommits anySatisfy: [ :each |
		(localCommits includes: each) not ]
]

{ #category : #testing }
IceLocalBranch >> hasOutgoingCommits [
	
	self hasUpstream ifFalse: [ ^ false ].
	^ self hasOutgoingCommitsTo: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> hasOutgoingCommitsTo: aRemote [
	
	| localCommits remoteCommits |
	self tracksRemoteBranch
		ifFalse: [ ^ false ].
	
	localCommits := self commits.
	remoteCommits := self trackedBranch commits.
	^ localCommits anySatisfy: [ :each |
		(remoteCommits includes: each) not ]
]

{ #category : #'API-remotes' }
IceLocalBranch >> hasUpstream [
	
	^ self subclassResponsibility
]

{ #category : #comparing }
IceLocalBranch >> hash [

	^ self name hash
]

{ #category : #'API-remotes' }
IceLocalBranch >> incomingCommits [
	
	self hasUpstream ifFalse: [ ^ #() ].
	^ self incomingCommitsFrom: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> incomingCommitsFrom: aRemote [
	
	| remoteBranch |

	remoteBranch := aRemote remoteBranchNamed: self name ifAbsent: [ ^ #() ].
	^ remoteBranch newCommitWalk
		uptoCommit: self commit;
		commits
]

{ #category : #'API-remotes' }
IceLocalBranch >> incomingCommitsFromBranch: aBranch [
	
	^ aBranch newCommitWalk
		uptoCommit: self commit;
		commits
]

{ #category : #testing }
IceLocalBranch >> isRemote [

	^ false
]

{ #category : #'API-merging' }
IceLocalBranch >> mergeWith: aCommit [

	self subclassResponsibility
]

{ #category : #'API-merging' }
IceLocalBranch >> mergeWithBranch: aBranch [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceLocalBranch >> outgoingCommits [
	
	self hasUpstream ifFalse: [ ^ #() ].
	^ self outgoingCommitsTo: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> outgoingCommitsTo: aRemote [
	
	| remoteBranch walk |

	"If there is no remote tracking branch in the remote, we cannot calculate outgoing commits.
	All the commits in the branch are going to be outgoing, so I will take the first 20 to show."
	remoteBranch := aRemote remoteBranchNamed: self name ifAbsent: [ ^ self reducedOutgoingCommitsWhenNoRemote ].
	walk := self newCommitWalk.
	remoteBranch ifNotNil: [ 
		walk uptoCommit: remoteBranch commit ].
	^ walk commits
]

{ #category : #'API-remotes' }
IceLocalBranch >> pull [
	
	self pullFrom: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> pullFrom: aRemote [
	
	| remoteBranch |
	aRemote fetchBranch: self.
	remoteBranch := aRemote
		remoteBranchNamed: self name
		ifAbsent: [ ^ self ].
	self repository workingCopy mergeCommit: remoteBranch commit.
]

{ #category : #'API-remotes' }
IceLocalBranch >> push [
	
	self pushTo: self upstream remote
]

{ #category : #'API-remotes' }
IceLocalBranch >> pushTo: aRemote [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceLocalBranch >> reducedOutgoingCommitsWhenNoRemote [
	^ self newCommitWalk
		maxNumber: 20;
		commits
]

{ #category : #'API-remotes' }
IceLocalBranch >> setUpstream: anUpstreamBranch [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceLocalBranch >> setUpstreamRemote: aRemote [
	
	self setUpstream: (aRemote remoteBranchNamed: self name)
]

{ #category : #accessing }
IceLocalBranch >> trackedBranch [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceLocalBranch >> tracksRemoteBranch [
	
	^ self subclassResponsibility
]

{ #category : #'API-remotes' }
IceLocalBranch >> upstream [
	
	^ self subclassResponsibility
]
