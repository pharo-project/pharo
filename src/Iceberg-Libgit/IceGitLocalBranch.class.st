Class {
	#name : #IceGitLocalBranch,
	#superclass : #IceLocalBranch,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #'private-pushing' }
IceGitLocalBranch >> basicPushTo: aRemote [
	| gitRemote |
	
	gitRemote := (LGitRemote of: self repositoryHandle named: aRemote name) lookup.
	[ 
		| pushProgress |
		pushProgress := IcePushTransferProgress new.
		"Push branch"
		self 
			pushBranchToRemote: aRemote 
			gitRemote: gitRemote 
			progress: pushProgress.
		"Push tags"
		self 
			pushTagsToRemote: aRemote
			gitRemote: gitRemote
			progress: pushProgress.
			
		"Verify we have an stream"
		self setUpstreamIfMissing: aRemote ] 
	on: LGit_GIT_ENONFASTFORWARD 
	do: [ :e | 
		e resignalAs: (IceRemoteDesynchronized new
			remote: aRemote;
			yourself) ]
]

{ #category : #accessing }
IceGitLocalBranch >> branchNamePrefix [

	^ 'refs/heads/'
]

{ #category : #accessing }
IceGitLocalBranch >> commit [
	
	repository handleLibgitError: [ | gitCommit gitId |
		gitId := (self repositoryHandle lookupLocalBranch: self name) targetId.
		gitCommit := LGitCommit of: self repositoryHandle fromId: gitId.
		^ self repository commitFromGitCommit: gitCommit ]
]

{ #category : #accessing }
IceGitLocalBranch >> commit: anIceGitCommit [
	"Update the branch to point to a commit.
	If the branch is HEAD, check it out in disk also."

	repository handleLibgitError: [ | localBranch gitcommit |
		localBranch := self repositoryHandle lookupLocalBranch: self name.
		gitcommit := self repositoryHandle revparse: anIceGitCommit id.
		localBranch setTargetId: gitcommit id.
		self = repository head
			ifTrue: [ self repositoryHandle
					checkout_treeish: gitcommit
					opts:
						(LGitCheckoutOptions defaults
							checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_force;
							yourself) ]]
]

{ #category : #'API-branches' }
IceGitLocalBranch >> delete [
	repository handleLibgitError: [ | found |
		found := self repositoryHandle lookupLocalBranch: self name ifAbsent: [ nil ].
		self repositoryHandle deleteBranch: found ]
]

{ #category : #'file-access' }
IceGitLocalBranch >> fileSystem [
	
	^ self commit fileSystem
]

{ #category : #accessing }
IceGitLocalBranch >> gitRef [
	
	^ self name
]

{ #category : #'private-tracking' }
IceGitLocalBranch >> hasUpstream [
	
	self upstreamIfNone: [ ^ false ].
	^ true
]

{ #category : #merging }
IceGitLocalBranch >> mergeWithBranch: anIceGitRemoteBranch [ 
	self shouldBeImplemented.
]

{ #category : #accessing }
IceGitLocalBranch >> name [

	^ self fullname withoutPrefix: self branchNamePrefix
]

{ #category : #'private-pushing' }
IceGitLocalBranch >> pushBranchToRemote: aRemote gitRemote: gitRemote progress: pushProgress [
	
	gitRemote 
		pushWithRefSpec: (LGitRefSpec new
			source: self fullname;
			destination: self fullname;
			yourself)
		pushOptions: (LGitPushOptions defaults
			callbacks: ((LGitRemoteCallbacks withProvider: (IceCredentialsProvider defaultForRemote: aRemote)) 
				pushTransferProgress: pushProgress; 
				yourself);
			yourself)
]

{ #category : #'private-pushing' }
IceGitLocalBranch >> pushTag: tag toRemote: aRemote gitRemote: gitRemote progress: pushProgress [

	gitRemote 
		pushWithRefSpec: (LGitRefSpec new
			source: 'refs/tags/' , tag name;
			destination: 'refs/tags/' , tag name;
			yourself)
		pushOptions: (LGitPushOptions defaults
			callbacks: ((LGitRemoteCallbacks withProvider: (IceCredentialsProvider defaultForRemote: aRemote))
				pushTransferProgress: pushProgress;
				yourself);
			yourself)
]

{ #category : #'private-pushing' }
IceGitLocalBranch >> pushTagsToRemote: aRemote gitRemote: gitRemote progress: pushProgress [

	self tags do: [ :tag | 
		self 
			pushTag: tag 
			toRemote: aRemote 
			gitRemote: gitRemote 
			progress: pushProgress ]
]

{ #category : #'API-remotes' }
IceGitLocalBranch >> pushTo: aRemote [

	repository handleLibgitError: [ 
		self basicPushTo: aRemote ]
]

{ #category : #accessing }
IceGitLocalBranch >> repositoryHandle [
	
	<repositoryHandleAccessor>
	^ repository repositoryHandle
]

{ #category : #'API-remotes' }
IceGitLocalBranch >> setUpstream: anIceGitRemoteBranch [
	
	repository handleLibgitError: [ | localBranch |
		localBranch := self repositoryHandle lookupLocalBranch: self name.
		localBranch setUpstream: anIceGitRemoteBranch gitRef ]
]

{ #category : #'private-pushing' }
IceGitLocalBranch >> setUpstreamIfMissing: aRemote [
	
	self hasUpstream ifTrue: [ ^ self ].
	self setUpstream: (aRemote remoteBranchNamed: self gitRef)
]

{ #category : #accessing }
IceGitLocalBranch >> shortname [

	^ self name
]

{ #category : #accessing }
IceGitLocalBranch >> trackedBranch [
	
	| upstream |
	upstream := self upstream.
	upstream ifNil: [ 
		upstream := self setUpstream: (self lookupRemoteBranch: '/origin/', self name) ].
	
	^ upstream
]

{ #category : #tracking }
IceGitLocalBranch >> tracksRemoteBranch [
	
	^ self hasUpstream
]

{ #category : #'private-tracking' }
IceGitLocalBranch >> upstream [
	
	^ self upstreamIfNone: [ IceUpstreamNotConfigured signal ]
]

{ #category : #'private-tracking' }
IceGitLocalBranch >> upstreamIfNone: aBlock [
	repository handleLibgitError: [ | localBranch gitUpstream |
		localBranch := self repositoryHandle lookupLocalBranch: self name.
		gitUpstream := localBranch upstream.
		gitUpstream ifNil: [ ^ aBlock value ].
		^ IceGitRemoteBranch named: gitUpstream name inRepository: repository ]
]
