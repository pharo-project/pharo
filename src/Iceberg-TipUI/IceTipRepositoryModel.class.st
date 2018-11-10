"
I'm a model for IceRepository entries.
"
Class {
	#name : #IceTipRepositoryModel,
	#superclass : #IceTipEntityModel,
	#traits : 'TIceCopyCommitId + TWithBranchModel',
	#classTraits : 'TIceCopyCommitId classTrait + TWithBranchModel classTrait',
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accessing }
IceTipRepositoryModel class >> unknownBranchLabel [
	^ 'Unknown'
]

{ #category : #actions }
IceTipRepositoryModel >> addPackages: aCollection [

	IceTipStandardAction new 
		repository: self entity;
		message: 'Adding packages';
		onSuccessRepositoryModified;
		execute: [ 
			aCollection do: [ :each | 
				self entity workingCopy addPackageNamed: each name ] ]
]

{ #category : #actions }
IceTipRepositoryModel >> addRemoteNamed: aName url: anUrl [
	
	IceTipStandardAction new 
		repository: self entity;
		message: 'Adding remote';
		onSuccessRepositoryModified;
		execute: [ | remote |
			remote := IceGitRemote name: aName url: anUrl.
			self entity addRemote: remote.
			remote fetch ]
]

{ #category : #accessing }
IceTipRepositoryModel >> allBranchModels [
	^ self entity allBranches 
		collect: [ :each | 
			(IceTipBranchModel 
				repositoryModel: self 
				on: each) 
				beCached ]
]

{ #category : #accessing }
IceTipRepositoryModel >> availableRepairOptions [
	<noCache>
	
	^ IceTipRepairAction allAvailableFor: self entity
]

{ #category : #accessing }
IceTipRepositoryModel >> branchModel [
	^ (IceTipBranchModel 
		repositoryModel: self 
		on: self entity branch)
		beCached
]

{ #category : #accessing }
IceTipRepositoryModel >> branchModelFor: aBranch [
	^ (IceTipBranchModel 
		repositoryModel: self 
		on: aBranch)
		beCached
]

{ #category : #accessing }
IceTipRepositoryModel >> branchName [
	self entity isMissing ifTrue: [ ^ self class unknownBranchLabel ].
	
	^ self entity head description.
]

{ #category : #accessing }
IceTipRepositoryModel >> branches [
	^ self entity localBranches
]

{ #category : #checking }
IceTipRepositoryModel >> checkMissing [
	
	self isMissing ifTrue: [ 
		IceError signal: ('You have no repository atttached to project "{1}"' format: { self name }) ]
]

{ #category : #actions }
IceTipRepositoryModel >> checkoutNewBranchNamed: aString inCommit: aCommit thenDo: aBlock [
	<noCache>

	IceTipStandardAction new 
		repository: self entity;
		message: ('Checking out new branch {1}' format: { aString });
		onSuccessRepositoryModified;
		execute: [ | branch |
			branch := self entity createBranch: aString inCommit: aCommit.
			aBlock value: branch ]
]

{ #category : #accessing }
IceTipRepositoryModel >> commit [
	
	^ self entity workingCopy referenceCommit
]

{ #category : #actions }
IceTipRepositoryModel >> commit: aCommitIssue then: aBlock [

	aCommitIssue 
		repository: self entity;
		onSuccess: aBlock;
		execute
]

{ #category : #accessing }
IceTipRepositoryModel >> commitId [
	^ [ self entity headCommit id ] on: Error do: [ nil ].
]

{ #category : #accessing }
IceTipRepositoryModel >> commitModelFor: aCommit [
	^ (IceTipCommitModel 
		repositoryModel: self 
		on: aCommit)
		beCached
]

{ #category : #operations }
IceTipRepositoryModel >> createSourceDirectory [
	<noCache>
	entity createSourceDirectory
]

{ #category : #actions }
IceTipRepositoryModel >> delete [
	<noCache>
	
	IceTipStandardAction new
		repository: self entity;
		message: ('Deleting {1}' format: {self name});
		execute: [ self entity delete ]
]

{ #category : #accessing }
IceTipRepositoryModel >> descriptionWithDecoration [
	| text |
	
	text := self displayString asText.
	self entity isModified 
		ifTrue: [ text := ('*', text) asText makeAllColor: self colorModified ].
	^ text
]

{ #category : #actions }
IceTipRepositoryModel >> discardChanges [
	<noCache>
	
	IceTipStandardAction new 
		repository: self entity;
		message: 'Discarding changes';
		onSuccessRepositoryModified;
		execute: [ 
			Iceberg announcer suspendAllWhile: [ 
				self entity discardChanges ] ]
]

{ #category : #actions }
IceTipRepositoryModel >> edit [
	<noCache>
	
	(IceTipEditProjectDialog on: self entity project) openDialogWithSpec
]

{ #category : #actions }
IceTipRepositoryModel >> fetch [
	<noCache>
	
	IceTipStandardAction new 
		repository: self entity;
		message: ('Fetching incoming commits of {1}' format: { self name });
		onSuccessRepositoryModified;
		execute: [ self entity fetch ]
]

{ #category : #actions }
IceTipRepositoryModel >> forget [
	<noCache>
	
	IceTipStandardAction new
		repository: self entity;
		message: ('Forgeting {1}' format: {self name});
		execute: [ self entity forget ]
]

{ #category : #testing }
IceTipRepositoryModel >> hasRemotes [
	^ self entity remotes notEmpty
]

{ #category : #testing }
IceTipRepositoryModel >> hasUnbornProject [
	^ self entity workingCopy project isUnborn
]

{ #category : #accessing }
IceTipRepositoryModel >> headModel [
	
	| head |
	head := self entity head.
	^ head isBranch
		ifTrue: [ self branchModelFor: head ]
		ifFalse: [ self commitModelFor: head ]
]

{ #category : #accessing }
IceTipRepositoryModel >> incomingCommits [
	^ self entity incomingCommits
]

{ #category : #accessing }
IceTipRepositoryModel >> info [ 
	^ { 
		'Branch' -> self entity branch name.
		'Location' -> self entity location pathString.
		'Upstream' -> self entity upstream asString.
	  }
]

{ #category : #testing }
IceTipRepositoryModel >> isCodeMissing [
	^ entity isCodeMissing
]

{ #category : #testing }
IceTipRepositoryModel >> isDetached [
	^ self entity workingCopy isDetached 
		or: [ self entity head isDetached ]
]

{ #category : #'private testing' }
IceTipRepositoryModel >> isLoaded [
	^ self entity loadedPackages notEmpty 
]

{ #category : #testing }
IceTipRepositoryModel >> isMissing [
	^ self entity isMissing
]

{ #category : #accessing }
IceTipRepositoryModel >> location [
	
	^ entity location
]

{ #category : #accessing }
IceTipRepositoryModel >> longStatus [

	(entity isMissing and: [ entity location isNil ])
		ifTrue: [ ^ 'Not configured git repository. Please clone the repository or change the repository location to an existing location' ].
	entity isMissing 
		ifTrue: [ ^ 'Cannot find a git repository at {1}. Please clone the repository again or change the repository location to an existing location' format: { entity location } ].
				
	entity workingCopy workingCopyState isUnknownCommitState 
		ifTrue: [ ^ 'This repository was created from commit {1} but the commit is not in your disk repository. You may fetch the correct commit from a remote repository or discard the code in your image and load what is in the repository.' format: { entity workingCopy referenceCommit shortId } ].	
	entity workingCopy isDetached 
		ifTrue: [ ^ 'The working copy commit {1} does not correspond with the repository commit {2}. You may synchronize them to continue: discard your image changes or change the repository to point to your commit.' format: { entity workingCopy referenceCommit shortId. entity headCommit shortId } ].
	entity head isDetached 
		ifTrue: [ ^ 'The repository is in detached head state in commit {1}. You may checkout a branch to continue.' format: { entity headCommit shortId } ].

	entity workingCopy project isUnborn
		ifTrue: [ ^ 'Cannot find a project in your repository. Please edit your repository meta-data to indicate the source code directory or change a location to a valid iceberg repository' ].


	^ 'Your repository seems ok. Please report a ticket in Iceberg''s issue tracker. If you can, please send us a backup of your image in this state for debugging.'
]

{ #category : #testing }
IceTipRepositoryModel >> matches: aString [
	^ self entity name matchesRegexIgnoringCase: aString
]

{ #category : #accessing }
IceTipRepositoryModel >> name [
	^ self entity name
]

{ #category : #accessing }
IceTipRepositoryModel >> numberOfIncomingCommits [
	^ self incomingCommits size
]

{ #category : #accessing }
IceTipRepositoryModel >> numberOfOutgoingCommits [
	^ [ | size |
		self entity branch hasUpstream ifFalse: [ ^ '*' ].
		size := self outgoingCommits size.
		size > 100
			ifTrue: [ '99+' ]
			ifFalse: [ size ] ]
		on: IceNotInBranch do: [ :e | 0 ]
]

{ #category : #accessing }
IceTipRepositoryModel >> outgoingCommits [
	^ self entity outgoingCommits
]

{ #category : #accessing }
IceTipRepositoryModel >> packages [
	^ self entity workingCopy packages
]

{ #category : #accessing }
IceTipRepositoryModel >> pushRemoteName [

	^ self entity branch upstream name
]

{ #category : #accessing }
IceTipRepositoryModel >> remoteModels [
	^ self entity remotes collect: [ :each |
		(IceTipRemoteModel repositoryModel: self on: each) beCached ]
]

{ #category : #accessing }
IceTipRepositoryModel >> remotes [
	
	^ entity remotes
]

{ #category : #accessing }
IceTipRepositoryModel >> repositoryModel [
	^ self
]

{ #category : #accessing }
IceTipRepositoryModel >> repositoryModelsByGroup [
	| branches tags |
	branches := self allBranchModels.
	tags := self tagModels.
	^ { 
	"Branches group"
	(IceTipTreeRepositoryGroup on: self)
		name: 'Branches';
		icon: (self iconNamed: #branch);
		children: (branches
			select: [ :each | each entity isLocal ]
			thenCollect: [ :each | IceTipTreeBranch on: each ]);
		yourself.
	"Remotes group"
	(IceTipTreeRepositoryGroup on: self)
		name: 'Remotes';
		icon: (self iconNamed: #remote);
		children: (self entity remotes collect: [ :eachRemote | | remoteModel |
			remoteModel := (IceTipRemoteModel repositoryModel: self on: eachRemote) beCached.
			(IceTipTreeRemote on: remoteModel)
				name: remoteModel name;
				children: (branches 
					select: [ :each | 
						each entity isRemote 
						and: [ each entity remoteName = eachRemote name ] ]
					thenCollect: [ :each | IceTipTreeBranch on: each ]);
				yourself	]);
		yourself.
	"Tags group"
	(IceTipTreeRepositoryGroup on: self)
		name: 'Tags';
		icon: (self iconNamed: #glamorousBookmark);
		children: (tags collect: [ :each | IceTipTreeTag on: each ]);
		yourself.
	}
]

{ #category : #accessing }
IceTipRepositoryModel >> shortCommitId [
	^ [self entity headCommit shortId] on: Error do: [ nil ].
]

{ #category : #accessing }
IceTipRepositoryModel >> status [
	[
		| status incoming outgoing | 

		self verifyDirectoryStructureIfMissing: [ :message | ^ message ].
		
		entity workingCopy workingCopyState isUnknownCommitState 
			ifTrue: [ ^ self toErrorMessage: entity workingCopy workingCopyState description ].	
		entity workingCopy isDetached 
			ifTrue: [  ^ self toErrorMessage: 'Detached Working Copy' ].
		(entity head isDetached and: [ entity head tags notEmpty ])
			ifTrue: [ ^ self toWarningMessage: 'Detached HEAD' ].
		entity head isDetached 
			ifTrue: [ ^ self toErrorMessage: 'Detached HEAD' ].

		entity workingCopy project isUnborn
			ifTrue: [ ^ self toErrorMessage: 'No Project Found' ].

		self isLoaded ifFalse: [ ^ 'Not loaded' ].

		status := OrderedCollection new.
		entity isModified ifTrue: [ status add: 'Uncommited changes' ].
		
		incoming := self incomingCommits size.
		incoming > 0 ifTrue: [ status add: ('{1} incoming' format: { incoming })].
		
		outgoing := self outgoingCommits size.
		outgoing > 0 ifTrue: [ status add: ('{1} not published' format: { outgoing })].

		status ifEmpty: [ status add: 'Up to date' ].
		
		^ ', ' join: status
	] 
	on: Error do: [ :error | ^ self toErrorMessage: error description ]

]

{ #category : #accessing }
IceTipRepositoryModel >> tagModels [
	
	^ self entity tags 
		collect: [ :each | 
			(IceTipTagModel 
				repositoryModel: self 
				on: each) 
				beCached ]
]

{ #category : #actions }
IceTipRepositoryModel >> updateLocation: aReference [
	
	"change location"
	self entity location: aReference.
	self entity postFetch.

	Iceberg announcer announce: (IceRepositoryModified for: self entity)
]

{ #category : #private }
IceTipRepositoryModel >> verifyDirectoryStructureIfMissing: aBlock [
	self entity isMissing 
		ifTrue: [ ^ aBlock value: (self toErrorMessage: 'Local repository missing') ]
]

{ #category : #accessing }
IceTipRepositoryModel >> workingCopyDiff [
	<noCache>

	^ IceTipWorkingCopyDiffModel repositoryModel: self on: self entity
]
