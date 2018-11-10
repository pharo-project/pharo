"
I represent an iceberg repository. I am an abstract class, my subclasses will represent concrete repositories such as monticello or git repositories. I define the main API and I have some common methods defined as template methods.

! Instantiation
Check my subclasses to know how to instantiate them.

! Basic  repository properties

All repositories have two main properties: a name and a working copy. 
 - name is used to identify a repository
 - the working copy is used to manage the status of all code loaded from the repository. See #IceWorkingCopy

! The repository HEAD

Moreover, every repository has to define a HEAD. The repository HEAD is a reference to our current working commit/branch in the repository.

  repository head.

The head can be either a branch or a commit. To access the current commit you can ask the head for its commit, or directly the repository for its headCommit (Yes, commits and branches are polymorphic, a commit returns self when asked for its commit).

  repository head commit.
  repository headCommit.

HEAD is an important concept inside iceberg. It is used to calculate if our code is synchronized, if we have commits to fetch and load, and so on...

Moreover, please notice that HEAD is a repository property, and it is completely separate from the image's state. The image can have code loaded from commits other than HEAD, and vice-versa. More of it is explained in IceWorkingCopy class comment.

! Branches

An iceberg repository natively  supports branches. To query the branches in the repository you can ask for:

  repository localBranches.
  repository remoteTrackedBranches.
  repository allBranches.

Other convenience methods also provide:

  repository localBranchNames.
  repository remoteBranchNames.

Take into account that such methods will return only already fetched branches. If the branch exists in a remote repository, you should need to do a #fetch before being able to see it.

Branches can also be queried by name, using the utility methods

  repository branchNamed: aName.
  repository branchNamed: aName ifAbsent: [ ... ].
  repository branchNamed: aName ifPresent: [ ... ].
  repository branchNamed: aName ifAbsent: [ ... ] ifPresent: [ ... ].

and the variant for remote branches.

  repository remoteBranchNamed: aName.
  repository remoteBranchNamed: aName ifAbsent: [ ... ].
  repository remoteBranchNamed: aName ifPresent: [ ... ].
  repository remoteBranchNamed: aName ifAbsent: [ ... ] ifPresent: [ ... ].

Branches can be created from a specific commit or, by default, from #headCommit.

  repository createBranch: aBranchName.
  repository createBranch: aBranchName inCommit: aCommit.

Notice that creating a branch will switch to it automatically.

The variant #ensureBranch: will create a branch only if a branch with the same name does not exist.

  repository ensureBranch: aBranchName.

Also, to create a branch tracking a remote branch (that is, that will start from the same commit and follow the evolution of a remote branch), you can use #createBranchTracking: aBranchName.

Finally, to switch to an existing branch:

  repository switchToBranchNamed: aBranchName.

! Remotes

Remotes are references to remote repositories. Remotes are used for pulling and pushing commits and branches.
Remotes can be added using the #addRemote: message and removed using the remoteRemote: message . However, thou shall be careful, remotes should be created from the class corresponding to the repository. For example, git repositories should use git remotes, and so on. Check IceRemote and its concrete subclasses.

  repository addRemote: aRemote.

Once remotes are configured, we can configure them as pull remote and push remote. A pull remote is a remote used for pulling commits by default, while a push remote is a remote used for pushing new commits by default.

  repository pullRemote: aRemote.
  repository pushRemote: aRemote.

  repository isPullRemote: aRemote.
  repository isPushRemote: aRemote.

If pull or push remotes are not configured, they will default to 'origin'. Origin is a well known remote, for which an accessor to the 'origin' remote  is provided through the #origin message.

  repository origin.

Remotes can be queried with:

  repository remotes.
  repository remoteNamed: aName.
  repository remoteNamed: aName ifAbsent: [ ... ].

Moreover, we can query if the current branch has commits to be uploaded using the #outgoingCommits and friends, or if the current branch has commits to be downloaded using the #ingoingCommits and friends.

  repository hasIncomingCommits.
  repository incomingCommits.
  repository hasIncomingCommitsFrom: aRemote.
  repository incomingCommitsFrom: aRemote.

  repository hasOutgoingCommits.
  repository outgoingCommits.
  repository hasOutgoingCommitsTo: aRemote.
  repository outgoingCommitsTo: aRemote.

Finally, we can interact with remotes via the fetch, pull and push operations.
Fetch downloads new commits and branches from the specified remote.
Pull and push operations delegate to pull and push of the current branch. Check IceLocalBranch.

  repository fetch.
  repository pull.
  repository push.

! Committing code

Once modifications are done, we can commit them to the repository using the commit* messages. The main API for it is #commitWithMessage: that will commit all changes with the given commit.

  repository commitWithMessage: 'aMessage in a string'.

A second commit message is also available to commit cherry picks:

  repository commitCherryPick: aChangesList withMessage: ' a message in a string'.

NOTICE that commits can only be done if the following is true:

 - HEAD is a branch
 - the working copy reference commit is the same commit as #headCommit

In any other case, commits are forbidden as they may break the repository consistency.

! Loading code & Package querying

From the repository point of view, only some convenience methods are provided to checkout all code from the current branch, unload all loaded code, know what packages are loaded or modified. Please refer to IceWorkingCopy for a more complete API.


"
Class {
	#name : #IceRepository,
	#superclass : #Object,
	#instVars : [
		'name',
		'workingCopy',
		'index',
		'commitsInPackageCache'
	],
	#classVars : [
		'Registry'
	],
	#category : #'Iceberg-Core'
}

{ #category : #testing }
IceRepository class >> isAbstract [

	^ self == IceRepository
]

{ #category : #registry }
IceRepository class >> registerRepository: aRepository [
	
	self registry add: aRepository.
	^ aRepository
]

{ #category : #registry }
IceRepository class >> registeredRepositoryIncludingPackage: aPackage [ 
	^ self registry 
		detect: [ :each | each includesPackageNamed: aPackage name ]
		ifNone: [ nil ]
]

{ #category : #registry }
IceRepository class >> registry [
	^ Registry ifNil: [ Registry := OrderedCollection new ]
]

{ #category : #registry }
IceRepository class >> reset [
	Registry := nil.
	Iceberg announcer announce: IceRepositoryForgotten new.
]

{ #category : #registry }
IceRepository class >> unregisterRepository: aRepository ifAbsent: aBlock [
	^ self registry 
		remove: aRepository 
		ifAbsent: aBlock 
]

{ #category : #'API-remotes' }
IceRepository >> addRemote: aRemote [  
	"Add a remote to the current repository.
	
	Throw an exception if a remote with the same name already exists"
	
	(self hasRemoteNamed: aRemote name)
		ifTrue: [ IceRemoteAlreadyExistsError new
			remoteName: aRemote name;
			signal ].
	
	self remoteAt: aRemote name put: aRemote.
	aRemote localRepository: self.
]

{ #category : #'API-branches' }
IceRepository >> allBranches [
	"Returns all local and remote branches, instances of IceBranch, from the current repository. This method will return only already fetched branches."
	
	self subclassResponsibility
]

{ #category : #'API-accessing' }
IceRepository >> branch [
	"Returns head if head is a branch.
	Throws an exception if HEAD is not a branch"
	
	self head validateIsBranch.
	^ self head
]

{ #category : #'API-accessing' }
IceRepository >> branchName [
	"Returns current branch name.
	
	Throws an exception if HEAD is not a branch"

	^ self branch name
]

{ #category : #'API-branches' }
IceRepository >> branchNamed: aName [
	"Access a branch by name.
	Throws an IceBranchNotFound is the branch does not exist in the repository."
	
	^ self
		branchNamed: aName
		ifPresent: [ :branch | ^ branch ]
		ifAbsent:[ 
			IceBranchNotFound new
				branchName: aName;
				signal ]
]

{ #category : #'API-branches' }
IceRepository >> branchNamed: aName ifAbsent: absentBlock [
	"Access a branch by name.
	Returns the result of the second argument if absent."
	
	^ self
		branchNamed: aName
		ifPresent: [ :branch | branch ]
		ifAbsent: absentBlock
]

{ #category : #'API-branches' }
IceRepository >> branchNamed: aString ifPresent: aBlockClosure [ 
	"Access a branch by name.
	If present, evaluate the block used as second argument with the branch found.
	If absent, do nothing."
	
	^ self
		branchNamed: aString
		ifPresent: aBlockClosure
		ifAbsent: [ self ]
]

{ #category : #'API-branches' }
IceRepository >> branchNamed: aName ifPresent: presentBlock ifAbsent: absentBlock [
	"Access a branch by name.
	If present, evaluate the block used as second argument with the branch found.
	If absent, evaluate the absent block."
	
	self subclassResponsibility
]

{ #category : #deprecated }
IceRepository >> branchesForMerge: aCommit [
	"Returns a list of local branches to which we could merge the received commit"

	^ self allBranches reject: [ :each | each includesCommit: aCommit ]
]

{ #category : #testing }
IceRepository >> canPush [
	
	^ self head isDetached not and: [ 
		self branch tracksRemoteBranch ]
]

{ #category : #'API-registry' }
IceRepository >> checkForRegistryConflicts [

	self subclassResponsibility
]

{ #category : #'API-working copy' }
IceRepository >> checkout: aCheckoutStrategy [
	
	self workingCopy checkout: aCheckoutStrategy
]

{ #category : #'API-loading' }
IceRepository >> checkoutAllPackages [

	"A checkout operation loads all packages from the current branch in the image"
	self branch checkoutAllPackages
]

{ #category : #'API-loading' }
IceRepository >> checkoutBranch: branchName [
	"Convenience method to checkout all packages from a branch given a branch name"

	(self
		branchNamed: branchName
		ifAbsent: [
			self createBranch: branchName ]) checkout
]

{ #category : #'API-history' }
IceRepository >> commitAt: commitId [
	
	^ self lookupCommit: commitId
]

{ #category : #'API-commit' }
IceRepository >> commitChanges: changes withMessage: message [
	"Creates a commit with the given changes using the comment given as argument.
	
	NOTICE that commits can only be done if the following is true:

 - HEAD is a branch
 - the working copy reference commit is the same commit as #headCommit"

	^ self workingCopy
		commitChanges: changes
		withMessage: message
]

{ #category : #'API-commit' }
IceRepository >> commitIndexWithMessage: message andParents: parentCommitishList [
	"Low level.
	
	Commit what is saved in the index"
	
	| newCommit |
	newCommit := index commitWithMessage: message andParents: parentCommitishList.
	index := self newIndex.
	self workingCopy referenceCommit: newCommit.
	self workingCopy refreshDirtyPackages.
	^ newCommit
]

{ #category : #'API-commit' }
IceRepository >> commitWithMessage: message [
	"Creates a commit with all changed code using the comment given as argument.
	
	NOTICE that commits can only be done if the following is true:

 - HEAD is a branch
 - the working copy reference commit is the same commit as #headCommit"
	
	^ self workingCopy commitWithMessage: message
]

{ #category : #'API-accessing' }
IceRepository >> commitishNamed: aName [
	"Resolve a name in the form of:
	  - a commit hash
	  - a branch name
	  - some other expression such as HEAD or master~1
	and return the corresponding iceberg object for it."

	^ self subclassResponsibility
]

{ #category : #'private-commits' }
IceRepository >> commitsInPackageCache [

	^ commitsInPackageCache ifNil: [ commitsInPackageCache := LRUCache new maximumWeight: 30  ]
]

{ #category : #'private-commits' }
IceRepository >> commitsInPackageCache: aCache [

	commitsInPackageCache := aCache
]

{ #category : #'API-branches' }
IceRepository >> createBranch: branchName [
	"Creates a new branch from #headCommit and switch to it"
	
	^ self createBranch: branchName inCommit: self headCommit
]

{ #category : #'API-branches' }
IceRepository >> createBranch: branchName inCommit: aCommit [
	"Creates a new branch from a given commit and switch to it"

	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> createNewBranch: branchName [
	"Creates a new branch starting on HEAD commit and switch to it"

	self createBranch: branchName inCommit: self headCommit
]

{ #category : #'API-tags' }
IceRepository >> createTag: aString [

	^ self workingCopy referenceCommit createTag: aString
]

{ #category : #'API-registry' }
IceRepository >> delete [
	"Delete is idempotent. If the repository does not exist, do nothing.
	Otherwise, forget the repository"
	self forget
]

{ #category : #printing }
IceRepository >> description [
	^ self origin url, '[', self head description, ']'
]

{ #category : #'API-changes' }
IceRepository >> discardChanges [
	
	self workingCopy discardChanges
]

{ #category : #'private-remotes' }
IceRepository >> doRemoveRemote: aRemote [
	
	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> ensureBranch: branchName [
	(self hasLocalBranchNamed: branchName)
		ifFalse: [ self createBranch: branchName ]

]

{ #category : #'API-remotes' }
IceRepository >> fetch [
	"Fetch all commits and branches from all remotes"
	
	self remotes do: [ :each | each fetch ] 
		displayingProgress: [ :each | 'Remote: ' , each name asString ]
]

{ #category : #'API-remotes' }
IceRepository >> fetchFrom: aRemote [ 
	"Fetch all commits and branches from the pull remote"

	aRemote fetch
]

{ #category : #'API-registry' }
IceRepository >> forget [
	self class unregisterRepository: self ifAbsent: [ ^ nil ].
	Iceberg announcer announce: (IceRepositoryForgotten repository: self)
]

{ #category : #'*Iceberg-Changes' }
IceRepository >> fullWorkingCopyDiff [
	^ IceDiff new
		source: self workingCopy;
		target: self head;
		buildFull;
		yourself
]

{ #category : #'API-remotes' }
IceRepository >> hasIncomingCommits [
	
	^ self branch hasIncomingCommits
]

{ #category : #'API-remotes' }
IceRepository >> hasIncomingCommitsFrom: aRemote [
	
	^ self branch hasIncomingCommitsFrom: aRemote
]

{ #category : #'API-branches' }
IceRepository >> hasLocalBraches [
	^ self localBranches isNotEmpty
]

{ #category : #'API-branches' }
IceRepository >> hasLocalBranchNamed: aString [
	
	^ self branchNamed: aString
		ifPresent: [ :branch | true ]
		ifAbsent: [ false ]
]

{ #category : #'API-remotes' }
IceRepository >> hasOutgoingCommits [
	
	^ self branch hasOutgoingCommits
]

{ #category : #'API-remotes' }
IceRepository >> hasRemoteNamed: aString [ 
	
	self remoteNamed: aString ifAbsent: [ ^ false ].
	^ true
]

{ #category : #'API-tags' }
IceRepository >> hasTagNamed: aString [ 
	
	self subclassResponsibility
]

{ #category : #'API-accessing' }
IceRepository >> head [
	"The repository HEAD is a reference to our current working commit/branch in the repository.

	The head can be either a branch or a commit. To access the current commit you can ask the head for its commit, or directly the repository for its #headCommit (Yes, commits and branches are polymorphic, a commit returns self when asked for its commit)."

	self subclassResponsibility
]

{ #category : #'API-accessing' }
IceRepository >> headCommit [
	"The head can be either a branch or a commit. To access the current commit you can ask the head for its commit, or directly the repository for its headCommit (Yes, commits and branches are polymorphic, a commit returns self when asked for its commit)."

	^ self head commit
]

{ #category : #printing }
IceRepository >> headDescription [
	
	^ self head description
]

{ #category : #'API-history' }
IceRepository >> includesCommit: anIceCommit [
	self lookupCommit: anIceCommit id ifAbsent: [ ^ false ].
	^ true	
]

{ #category : #'API-packages' }
IceRepository >> includesPackageNamed: aString [
	"Returns true if the given package name belongs to a package loaded in the image"
	
	^ self workingCopy includesPackageNamed: aString
]

{ #category : #'API-remotes' }
IceRepository >> incomingCommits [
	
	(self head isDetached)
		ifTrue: [ ^ #() ].
	^ self branch incomingCommits
]

{ #category : #accessing }
IceRepository >> index [
	^ index
]

{ #category : #initialization }
IceRepository >> initialize [
	super initialize.

	workingCopy := IceWorkingCopy basicNew.
	workingCopy repository: self.
	workingCopy initialize.
	
	index := self newIndex.
]

{ #category : #'private-commit' }
IceRepository >> internalStoreVersion: aMCVersion [ 
	
	self subclassResponsibility
]

{ #category : #testing }
IceRepository >> isCloneOfUrl: aString [ 

	^ self origin url = aString
]

{ #category : #'API-packages' }
IceRepository >> isModified [
	"Returns true if a package in the working copy has modifications"
	
	^ self workingCopy isModified
]

{ #category : #'API-packages' }
IceRepository >> loadedPackages [
	"Lists all loaded packages"

	^ self workingCopy loadedPackages
]

{ #category : #'API-branches' }
IceRepository >> localBranchNames [
	
	^ self localBranches collect: #name
]

{ #category : #'API-branches' }
IceRepository >> localBranches [
	"Lists local branches"
	
	self subclassResponsibility
]

{ #category : #accessing }
IceRepository >> log [
	^ self subclassResponsibility
]

{ #category : #'API-history' }
IceRepository >> lookupCommit: commitId [

	^ self
		lookupCommit: commitId
		ifAbsent: [ IceUnknownCommit new 
				repository: self;
				id: commitId; yourself ]
]

{ #category : #'API-history' }
IceRepository >> lookupCommit: commitId ifAbsent: aBlock [
	self subclassResponsibility
]

{ #category : #'*Iceberg-Adapters' }
IceRepository >> metacelloAdapter [
	^ IceMetacelloRepositoryAdapter for: self
]

{ #category : #'API-packages' }
IceRepository >> modifiedPackages [
	"Lists all modified packages currently loaded in the image"

	^ self workingCopy modifiedPackages
]

{ #category : #accessing }
IceRepository >> name [
	^ name
]

{ #category : #accessing }
IceRepository >> name: aString [
	"Change the name of the repository as is seen inside image (real project name resides 
	 externally, in repository). This can be confusing, but some times you need it to create new 
	 repositories from image (before creating repository externally... which means no remote)"
	name := aString
]

{ #category : #'API-history' }
IceRepository >> newCommitWalk [
	
	self subclassResponsibility
]

{ #category : #'private-changes' }
IceRepository >> newIndex [
	
	self subclassResponsibility
]

{ #category : #'API-commits' }
IceRepository >> newNoCommit [
	
	self subclassResponsibility
]

{ #category : #'API-project' }
IceRepository >> newProjectFromCommit: aCommit [

	self subclassResponsibility
]

{ #category : #'API-project' }
IceRepository >> newUnbornProject [
	
	^ IceUnbornProject onRepository: self
]

{ #category : #'API-changes' }
IceRepository >> notifyPackageModified: aString [ 
	
	^ self workingCopy notifyPackageModified: aString
]

{ #category : #'API-remotes' }
IceRepository >> origin [

	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceRepository >> outgoingCommits [
	^ self branch outgoingCommits 
]

{ #category : #'API-remotes' }
IceRepository >> outgoingCommitsTo: aRemote [
	^ self branch outgoingCommitsTo: aRemote
]

{ #category : #'API-packages' }
IceRepository >> packageNamed: packageName [
	"Returns the loaded package with the given name"
	
	^ self workingCopy packageNamed: packageName
]

{ #category : #'private-tags' }
IceRepository >> peelTag: anIceTag [ 
	
	self subclassResponsibility
]

{ #category : #'private-remotes' }
IceRepository >> postFetch [
	"This method is called after every fetch.
	Fox possible broken references using unknown commits."
	| maybeGoodCommit |
	
	self workingCopy referenceCommit isUnknownCommit ifFalse: [ ^ self ].
	maybeGoodCommit := self lookupCommit: self workingCopy referenceCommit id.
	maybeGoodCommit isUnknownCommit ifFalse: [ 	maybeGoodCommit adopt ]
]

{ #category : #printing }
IceRepository >> printOn: aStream [
	aStream nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: name asString;
		nextPut: $)
]

{ #category : #'API-project' }
IceRepository >> project [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceRepository >> pull [
	"Fetch new commits corresponding to the current branch from the default pull remote.
	Then merge the remote branch with the current branch."
	self branch pull
]

{ #category : #'API-remotes' }
IceRepository >> pullFrom: aRemote [
	"Fetch new commits corresponding to the remote.
	Then merge the remote branch with the current branch."
	self branch pullFrom: aRemote
]

{ #category : #'API-remotes' }
IceRepository >> push [
	"Push current commits to the default push remote"
	
	self branch push
]

{ #category : #'API-remotes' }
IceRepository >> pushTo: aRemote [	
	
	self branch pushTo: aRemote
]

{ #category : #'API-registry' }
IceRepository >> register [
	self checkForRegistryConflicts.
	self class registerRepository: self.
	Iceberg announcer announce: (IceRepositoryCreated repository: self).
	^ self 

]

{ #category : #'private-remotes' }
IceRepository >> remoteAt: aString put: aRemote [

	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> remoteBranchNamed: aString [
	"Access a remote branch by name.
	Throws an IceBranchNotFound is the branch does not exist in the repository."
	
	^ self
		remoteBranchNamed: aString
		ifPresent: [ :branch | ^ branch ]
		ifAbsent: [ IceBranchNotFound new
			branchName: aString;
			signal ]
]

{ #category : #'API-branches' }
IceRepository >> remoteBranchNamed: aName ifAbsent: absentBlock [
	"Access a remote branch by name.
	Returns the result of the second argument if absent."
	
	^ self
		branchNamed: aName
		ifPresent: [ :branch | branch ]
		ifAbsent: absentBlock
]

{ #category : #'API-branches' }
IceRepository >> remoteBranchNamed: aString ifPresent: aBlockClosure [ 
	"Access a remote branch by name.
	If present, evaluate the block used as second argument with the branch found.
	If absent, do nothing."
	
	^ self
		branchNamed: aString
		ifPresent: aBlockClosure
		ifAbsent: [ self ]
]

{ #category : #'API-branches' }
IceRepository >> remoteBranchNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [ 
	"Access a remote branch by name.
	If present, evaluate the block used as second argument with the branch found.
	If absent, evaluate the absent block."
	
	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> remoteBranchNames [
	
	^ self remoteTrackedBranches collect: #name
]

{ #category : #'API-remotes' }
IceRepository >> remoteNamed: aString [ 
	
	^ self remoteNamed: aString ifAbsent: [ IceRemoteNotFoundError new
		remoteName: aString;
		signal ]
]

{ #category : #'API-remotes' }
IceRepository >> remoteNamed: aString ifAbsent: aBlockClosure [ 
	
	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> remoteTrackedBranches [
	"Lists branches in remotes"
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceRepository >> remotes [
	
	self subclassResponsibility
]

{ #category : #'API-remotes' }
IceRepository >> removeRemote: aRemote [
	
	(self hasRemoteNamed: aRemote name)
		ifFalse: [ IceRemoteNotFoundError new
			remoteName: aRemote name;
			signal ].
		
	self doRemoveRemote: aRemote
]

{ #category : #'API-tags' }
IceRepository >> removeTag: anIceTag [ 
	
	self subclassResponsibility
]

{ #category : #'API-accessing' }
IceRepository >> setHead: anIceGitBranch [ 
	
	self subclassResponsibility
]

{ #category : #'API-accessing' }
IceRepository >> subdirectoryPath [
	"Return a relative path containing the path to the code source.
	This is required to compute the diff between two versions.
	
	Subclasses that do not require this may implement it, for example, as an empty relative path"
	
	self subclassResponsibility
]

{ #category : #'API-branches' }
IceRepository >> switchToBranchNamed: aString [
	
	^ (self branchNamed: aString)
		switch;
		yourself
]

{ #category : #'API-branches' }
IceRepository >> switchToCommitishNamed: aString [
	
	^ (self commitishNamed: aString)
		switch;
		yourself
]

{ #category : #'API-tags' }
IceRepository >> tagNamed: aString [ 
	
	(self hasTagNamed: aString) ifFalse: [ 
		IceTagNotFound new
			tagName: aString;
			signal ].
		
	^ IceTag named: aString inRepository: self
]

{ #category : #'API-tags' }
IceRepository >> tagNames [
	
	^ self tags collect: #name
]

{ #category : #'API-tags' }
IceRepository >> tags [
	
	self subclassResponsibility
]

{ #category : #'API-loading' }
IceRepository >> unload [
	"Unload all code loaded into the image that belong to this repository"
	
	self workingCopy unload
]

{ #category : #validating }
IceRepository >> validateCanCommit [
	"Private API. Do not rely on this.
	
	To know if you can commit you should ask the working copy"
	
	"If the repository head is detached (aka not a branch) we cannot commit.
	The repository has to be placed on a branch."
	self head isDetached ifTrue: [
		IceWorkingCopyDesyncronized signal ]
]

{ #category : #validating }
IceRepository >> validateCanPull [
	self isModified 
		ifTrue: [ IceShouldCommitBeforePull signal ]
]

{ #category : #validating }
IceRepository >> validateCanPushTo: aRemote [
	| commits |
	
	"I need to perform a fetch to have access to latest commits, because there is no 
	 other way to know if local repository is behind remote repository."
	aRemote fetch.
	"Now I can check if we are in sync"
	commits := self branch incomingCommitsFrom: aRemote.
	commits ifEmpty: [ ^ self ].

	IceRemoteDesynchronized new 
		asResumable;
		remote: aRemote;
		signal
]

{ #category : #'private-packages' }
IceRepository >> workingCopies [
	^ self loadedPackages collect: #workingCopy

]

{ #category : #accessing }
IceRepository >> workingCopy [
	"See IceWorkingCopy"
	
	^ workingCopy
]

{ #category : #accessing }
IceRepository >> workingCopy: aWorkingCopy [
	"See IceWorkingCopy"
	
	workingCopy := aWorkingCopy.
	workingCopy repository: self
]

{ #category : #'as yet unclassified' }
IceRepository >> workingCopyDiff [
	
	^ self workingCopy diffToReferenceCommit
]
