"
I represent and manage all code that is loaded in the current image. My main responsibilities are the bookeeping of loaded packages, the loaded commits, the modification and diffs, and the main entry point for committing code.

! Adding and Loading Packages

Adding a working copy to a repository is achieved by the message addPackageNamed:

  repository addPackageNamed: aPackageNameString.

I can be later on queried using the following messages

  repository includesPackageNamed: aPackageNameString.
  repository isLoadedPackageNamed: aPackageNameString.

  repository packageNames.

Packages already in a repository can be loaded and unloaded with:

  repository checkoutPackageNamed: aPackageNameString.
  repository unloadPackageNamed: aPackageNameString.

And queried with:

  repository loadedPackages.

! The Working Copy State

The working copy can be in different states depending on the repository and the package. It is the working copy state reponsibility to decide wether we can commit, if we are on a merge, and so on... The working copy state can be obtained through the message #workingCopyState.

  workingCopy workingCopyState.

The working copy state is calculated every time that it is called. This is because the state of the repository can be modified from outside the system (e.g.,  the command line or another tool). In any case, calculating the working copy state is fast enough to be executed on-line even for big repositories such as Pharo's. The working copy state is calculated from the status of each of its packages. It was decided like this because it may happen that somebody downloads a package from different commits. If this situation changes in the future, this is a good point for simplification.

Working copy states are instances of IceWorkingCopyState. Check it and its subclasses for more information.

! The Current Commit(s)

A crucial information bookeept by the working copy is the current loaded commit(s). Indeed, depending on the state of the packages and the loaded code, there may be a single current commit or several:

 - having a single commit means that all loaded packages were loaded from the same commit
 - having multiple commits mean that we are in a merge situation
 - having no commit means that no packages were loaded or that they were loaded from another mechanism such as monticello 

In general terms, the current commit should not be accessed directly.

The following methods allow for testing and accessing the current commits:

  repository hasSingleCommit.
  repository hasUnknownCommit.
  repository isInMerge.

  repository referenceCommit.
  repository referenceCommits.

!  Changes and Committing

The working copy bookkeeps the loaded packages and their state and thus it provides methods to query and commit those changes into the repository.

  repository modifiedPackages.
  repository isModified.

  repository commitWithMessage: aMessageString.
  repository commitChanges: aListOfChanges withMessage: aMessagString.
  repository commitPackages: aListOfPackages withMessage: aMessageString.

NOTICE that commits can only be done if the following is true:

 - repositroy HEAD is not detached (i.e., it is a branch)
 - I am not detached. I.e., my reference commit (in case of a merge, it should be the first one) is the same commit as #headCommit

Such validations can be verified with the following messages

  repository validateCanCommit.
  repository isDetached.

!  Merging

The main entry point for merging a commit is the working copy, through the method #mergeCommit:. This method will, delegating to the working copy state, verify that the commit needs to be merged, can be merged, and if it has conflicts. In case there are no conflicts an automatic merge will be issued by default.

  workingCopy mergeCommit: aCommit.

IMPORTANT IMPLEMENTATION NOTE. The version information and commits are so far stored inside MCWorkingCopy and co. Check #updatePackage:toCommits:
"
Class {
	#name : #IceWorkingCopy,
	#superclass : #Object,
	#instVars : [
		'repository',
		'packages',
		'referenceCommit',
		'shouldIgnoreNotifications',
		'project',
		'properties'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceWorkingCopy class >> fromWorkingCopy: aWorkingCopy [
	^ self basicNew 
		repository: aWorkingCopy repository; 
		initialize;
		project: aWorkingCopy project;
		yourself
]

{ #category : #'instance creation' }
IceWorkingCopy class >> repository: repository [
	^ self basicNew 
		repository: repository; 
		initialize;
		yourself
]

{ #category : #'API-packages' }
IceWorkingCopy >> addPackage: aPackage [
	
	self basicAddPackage: aPackage.
	aPackage beDirty.
]

{ #category : #'API-packages' }
IceWorkingCopy >> addPackageNamed: aString [ 
	
	self addPackage: (IcePackage named: aString asSymbol repository: repository) beDirty
]

{ #category : #'API-packages' }
IceWorkingCopy >> addPackages: aCollection [ 
	
	aCollection keysDo: [ :each | self addPackageNamed: each ]
]

{ #category : #accessing }
IceWorkingCopy >> adoptCommit: aCommit [

	| oldCommit |
	self referenceCommit = aCommit
		ifTrue: [ ^ self ].

	oldCommit := self referenceCommit.
	self referenceCommit: aCommit.
	
	"If both commits are from a different class but still have the same ID, we do not need to force the calculation of dirty packages.
	This is required for the case when a working copy existed with an unknown comit but it was not linked to a repository"
	(oldCommit isNoCommit not and: [ oldCommit id = aCommit id ])
		ifFalse: [ self forceCalculateDirtyPackages. ]
]

{ #category : #'private-monticello' }
IceWorkingCopy >> applyDefinition: anIceDefinition inNode: aNode [

	| loader |
	(anIceDefinition isFileDefinition
		or: [ anIceDefinition isDirectoryDefinition ])
			ifTrue: [ ^ self ].
	loader := MCPackageLoader new.
	loader addDefinition: anIceDefinition asMCDefinition.
	loader load
]

{ #category : #'API-packages' }
IceWorkingCopy >> basicAddPackage: aPackage [
	
	self packagesDictionary at: aPackage name put: aPackage.
]

{ #category : #'API-packages' }
IceWorkingCopy >> basicRemovePackage: aPackage [
	
	self packagesDictionary 
		removeKey: aPackage name 
		ifAbsent: []
]

{ #category : #'private-project' }
IceWorkingCopy >> basicSetProject: aProject [

	project := aProject
]

{ #category : #'API-changes' }
IceWorkingCopy >> canRevertChanges [
	
	^ true
]

{ #category : #'private-changes' }
IceWorkingCopy >> changesFromCommit: anIceCommit [
	"Image changes should be taken into account before changes in the repository"
	^ (self modifiedPackages collect: [ :each | 
			IceImageChange package: each ]) asArray,
				(self referenceCommit changesFromCommit: anIceCommit) asArray
]

{ #category : #'private-changes' }
IceWorkingCopy >> changesFromNoCommit: anIceNoCommit [ 
	
	^ self packages collect: [ :each | IceImageChange package: each ]
]

{ #category : #'API-changes' }
IceWorkingCopy >> changesTo: aCommitish [

	| removedPackages changes |
	changes := Set new.
	
	changes addAll: (aCommitish changesFromCommit: self referenceCommit).
	removedPackages := aCommitish packages \ self packages.
	changes addAll: (((self modifiedPackages, removedPackages) collect: [ :each | 
			IceImageChange package: each ])).
		
	self project isDirty
		ifTrue: [ changes add: IceProjectChange new ].
	self repositoryProperties isDirty
		ifTrue: [ changes add: IceCypressPropertiesChange new ].
	^ changes
]

{ #category : #'API-loading' }
IceWorkingCopy >> checkout: aCheckoutStrategy [
	
	aCheckoutStrategy checkoutOn: self
]

{ #category : #'API-commits' }
IceWorkingCopy >> commitChanges: aDiff withMessage: message [ 
	
	^ self commitChanges: aDiff withMessage: message force: false
]

{ #category : #'as yet unclassified' }
IceWorkingCopy >> commitChanges: aDiff withMessage: message force: forcing [ 

	"Creates a commit with the given changes using the comment given as argument.
	The forcing parameter allows to create an empty commit. This is used by the merge.
	
	NOTICE that commits can only be done if the following is true:

 - HEAD is a branch
 - the working copy reference commit is the same commit as #headCommit"

	| newCommit |
	self validateCanCommit.
	self repository index
		updateDiskWorkingCopy: aDiff;
		updateIndex: aDiff.

	(forcing not and: [repository index isEmpty]) 
		ifTrue: [ IceNothingToCommit signal ].

	newCommit := self repository
		commitIndexWithMessage: message
		andParents: (self workingCopyState referenceCommits reject: [ :each | each isNoCommit ]).
	^ newCommit
]

{ #category : #'API-commits' }
IceWorkingCopy >> commitWithMessage: aString [ 
	"Creates a commit with all changed code using the comment given as argument.
	
	NOTICE that commits can only be done if the following is true:

 - HEAD is a branch
 - the working copy reference commit is the same commit as #headCommit"
	
	^ self
		commitChanges: self repository workingCopyDiff
		withMessage: aString
]

{ #category : #'API-history' }
IceWorkingCopy >> commonAncestorWith: aCommitish [
	
	^ self referenceCommit commonAncestorWith: aCommitish
]

{ #category : #'API-history' }
IceWorkingCopy >> commonAncestorWithCommit:  aCommitish [
	
	^ self referenceCommit commonAncestorWithCommit: aCommitish
]

{ #category : #'API-changes' }
IceWorkingCopy >> diffTo: aCommitish [

	^ IceDiff
		from: self
		to: aCommitish commit
]

{ #category : #'API-changes' }
IceWorkingCopy >> diffToReferenceCommit [

	^ self diffTo: self referenceCommit
]

{ #category : #'API-changes' }
IceWorkingCopy >> discardChanges [
	
	self discardChanges: self diffToReferenceCommit tree
]

{ #category : #'API-changes' }
IceWorkingCopy >> discardChanges: aTree [
	
	self loadChangesInWorkingCopy: (aTree collect: [ :definition |
		definition inverse ]).
	self refreshDirtyPackages
]

{ #category : #accessing }
IceWorkingCopy >> fileSystem [
	
	^ repository location ifNil: [ FileSystem memory root ]
]

{ #category : #'API-changes' }
IceWorkingCopy >> forceCalculateDirtyPackages [

	| diff |
	diff := IceDiff new
		sourceVersion: self;
		targetVersion: self referenceCommit;
		buildFull.

	self loadedPackages do: [ :each |
		each beDirty: (diff includesPackageNamed: each name) ].
]

{ #category : #'API-commits' }
IceWorkingCopy >> hasSingleCommit [
	"Returns true if all loaded packages were loaded from the single same commit"
	
	^ self workingCopyState isSingleCommitState
]

{ #category : #'API-commits' }
IceWorkingCopy >> hasUnknownCommit [
	"Returns true if we couldn't calculate a single commit or a merge situation"
	
	^ self workingCopyState isUnknownCommitState
]

{ #category : #'private-packages' }
IceWorkingCopy >> ignoreNotificationsDuring: aBlock [
	"While the working copy is loading code, it should ignore modifications to avoid circular refreshes"

	| oldValue |
	oldValue := shouldIgnoreNotifications.
	shouldIgnoreNotifications := true.
	aBlock ensure: [ shouldIgnoreNotifications := oldValue ]
]

{ #category : #'private-packages' }
IceWorkingCopy >> includesInWorkingCopyPackageNamed: aString [ 
	^ (self packagesDictionary includesKey: aString)
		and: [ (self packageNamed: aString) isLoaded ]
]

{ #category : #'API-packages' }
IceWorkingCopy >> includesPackageNamed: aString [ 
	"true if is in image or is in commit"
	^ self includesInWorkingCopyPackageNamed: aString
]

{ #category : #initialization }
IceWorkingCopy >> initialize [

	super initialize.
	shouldIgnoreNotifications := false.
	
	packages := Dictionary new.
	referenceCommit := repository newNoCommit.
	self basicSetProject: referenceCommit project.
	properties := referenceCommit properties asBornProperties
]

{ #category : #initialization }
IceWorkingCopy >> initializePackagesFromRepository [
	"If no reference commit was configured, we adopt the head commit"
	| commitToAdopt |
	commitToAdopt := referenceCommit isNoCommit
		ifTrue: [ self repository headCommit ]
		ifFalse: [ referenceCommit ].
	self adoptCommit: commitToAdopt.
]

{ #category : #'API-merging' }
IceWorkingCopy >> isCommitMerged: aCommit [

	^ self workingCopyState isCommitMerged: aCommit
]

{ #category : #validating }
IceWorkingCopy >> isDetached [

	^ self workingCopyState isDetached
]

{ #category : #'API-commits' }
IceWorkingCopy >> isInMerge [
	"Returns true if all the repository packages are in merge between the same commits."
	
	^ self workingCopyState isInMerge
]

{ #category : #'API-packages' }
IceWorkingCopy >> isLoadedPackageNamed: aString [ 
	
	^ RPackageOrganizer default includesPackageNamed: aString
]

{ #category : #'API-changes' }
IceWorkingCopy >> isModified [
	^ self project isDirty or: [ self loadedPackages anySatisfy: [ :e | e isDirty ] ]
]

{ #category : #testing }
IceWorkingCopy >> isWorkingCopy [
	
	^ true
]

{ #category : #'API-loading' }
IceWorkingCopy >> loadAllPackages [
	
	self loadPackagesNamed: repository head packageNames
]

{ #category : #'private-changes' }
IceWorkingCopy >> loadChangesInWorkingCopy: aTree [
	
	| visitor |
	visitor := IceWorkingCopyPatcherVisitor on: self repository.
	aTree accept: visitor.
	
	self ignoreNotificationsDuring: [
		visitor load.
	]

]

{ #category : #'API-loading' }
IceWorkingCopy >> loadPackageNamed: aString [ 
	
	self loadPackagesNamed: { aString }
]

{ #category : #'API-loading' }
IceWorkingCopy >> loadPackageNamed: aString fromCommit: aCommit [

	self loadPackagesNamed: { aString } fromCommit: aCommit
]

{ #category : #'API-loading' }
IceWorkingCopy >> loadPackagesNamed: aCollection [ 
	
	self
		loadPackagesNamed: aCollection
		fromCommit: self repository head commit
]

{ #category : #'private-monticello' }
IceWorkingCopy >> loadPackagesNamed: aCollection fromCommit: aCommit [
	
	self ignoreNotificationsDuring: [ | loader |
		loader := MCVersionLoader new.
		aCollection do: [ :name | | package packageVersion |
			package := self packageNamed: name.
			packageVersion := (IceSavedPackageVersion
				fromCommit: aCommit
				package: package)
					mcVersion.
			loader addVersion: packageVersion.
			package beClean.
		].
		loader load ]
]

{ #category : #'API-loading' }
IceWorkingCopy >> loadedPackages [

	^ self packages select: #isLoaded
]

{ #category : #'API-packages' }
IceWorkingCopy >> markAllPackagesAsClean [
	
	self markAsCleanPackagesThat: [ :each | true ]
]

{ #category : #'API-packages' }
IceWorkingCopy >> markAsCleanPackagesNotInDiff: aDiff [
	
	self markAsCleanPackagesThat: [ :each |
		(aDiff includesPackageNamed: each name) not ]
]

{ #category : #'API-packages' }
IceWorkingCopy >> markAsCleanPackagesThat: aBlock [
	
	self modifiedPackages do: [ :each |
		(aBlock value: each) ifTrue: [ each beClean ] ]
]

{ #category : #'API-packages' }
IceWorkingCopy >> markAsDirtyPackageNamed: aPackageName [

	(self packageNamed: aPackageName) beDirty
	
	
]

{ #category : #'private-monticello' }
IceWorkingCopy >> mcVersionFor: anIceSavedPackageVersion [ 

	| mcPackage mcWorkingCopy |
	mcPackage := anIceSavedPackageVersion mcPackage.
	mcWorkingCopy := mcPackage workingCopy.
	^ MCVersion new
		setPackage: anIceSavedPackageVersion mcPackage
		info: mcWorkingCopy versionInfo
		snapshot: mcWorkingCopy snapshot
		dependencies: #()
]

{ #category : #'API-merging' }
IceWorkingCopy >> mergeCommit: aCommit [
	
	^ self workingCopyState mergeCommit: aCommit
]

{ #category : #'API-changes' }
IceWorkingCopy >> modifiedPackages [

	^ self loadedPackages select: [ :e | e isDirty ]
]

{ #category : #'API-packages' }
IceWorkingCopy >> notifyPackageModified: aString [ 
	self flag: #todo. "we cannot use #includesPackageNamed: as is because it can happen 
	that a package is present in a commit but not in image yet?"
	
	self shouldIgnoreNotifications ifTrue: [ ^ false ].
	
	(self includesInWorkingCopyPackageNamed: aString)
		ifTrue: [ | package |
			package := self packageNamed: aString.
			package isDirty ifFalse: [ 
				package beDirty.
				^ true ]].
	^ false
]

{ #category : #'API-packages' }
IceWorkingCopy >> packageNamed: aString [ 
	
	^ self packagesDictionary at: aString
]

{ #category : #'API-packages' }
IceWorkingCopy >> packageNames [
	
	^ self packagesDictionary keys
]

{ #category : #'API-packages' }
IceWorkingCopy >> packages [
	
	^ self packagesDictionary values
]

{ #category : #'private-packages' }
IceWorkingCopy >> packagesDictionary [

	^ packages
]

{ #category : #'API-project' }
IceWorkingCopy >> project [
	
	"For backwards compatibility, if we don't find a project, take the project from HEAD"
	^ project ifNil: [ self refreshProjectFromRepository ]
]

{ #category : #'API-project' }
IceWorkingCopy >> project: anIceProject [

	self basicSetProject: anIceProject.
	self refreshProject.
]

{ #category : #'API-properties' }
IceWorkingCopy >> properties [
	^ properties
]

{ #category : #'API-commits' }
IceWorkingCopy >> referenceCommit [
	
	^ referenceCommit
]

{ #category : #accessing }
IceWorkingCopy >> referenceCommit: aCommit [

	self referenceCommit = aCommit
		ifTrue: [ ^ self ].

	referenceCommit := aCommit.
	self refreshProjectFromRepository
]

{ #category : #'API-commits' }
IceWorkingCopy >> referenceCommits [
	"Returns a collection with all current loaded commits.
	If we are in a single commit state, return a collection with only one element."
	
	^ { referenceCommit }
]

{ #category : #'API-packages' }
IceWorkingCopy >> refreshDirtyPackages [
	self markAsCleanPackagesNotInDiff: self repository workingCopyDiff
]

{ #category : #'private-packages' }
IceWorkingCopy >> refreshPackages [
	| commitPackages |
	commitPackages := self referenceCommit isCollection
		ifTrue: [ (self referenceCommit flatCollect: [ :each | each packages ]) asSet ]
		ifFalse: [ self referenceCommit project packages ].

	"remove packages that are not there anymore"
	(self packages \ commitPackages) do: [ :each | self basicRemovePackage: each ].
	"Add packages not already there"
	(commitPackages \ self packages) do: [ :each | self basicAddPackage: each ].

]

{ #category : #'private-project' }
IceWorkingCopy >> refreshProject [

	repository commitsInPackageCache: nil.
	self refreshPackages
]

{ #category : #accessing }
IceWorkingCopy >> refreshProjectFromRepository [

	| commitToCheck maybeProject |
	commitToCheck := referenceCommit isCollection
		ifTrue: [ referenceCommit first ]
		ifFalse: [ referenceCommit ].

	maybeProject := commitToCheck project.
	maybeProject isInvalid ifFalse: [ 
		self basicSetProject: maybeProject ].
	
	properties := commitToCheck properties asBornProperties.
	self refreshProject.
	^ project
]

{ #category : #'API-properties' }
IceWorkingCopy >> refreshRepositoryProperties [
	
	properties := self referenceCommit properties asBornProperties.

]

{ #category : #'private-impacting-changes' }
IceWorkingCopy >> removeClassNamed: aString [ 
	
	"Impact the system.
	This should be enhanced"
	(Smalltalk globals at: aString ifAbsent: [ ^ self ]) removeFromSystem
]

{ #category : #'private-impacting-changes' }
IceWorkingCopy >> removeMethod: anIceMethodDefinition [ 
	
	anIceMethodDefinition asMCDefinition unload
]

{ #category : #accessing }
IceWorkingCopy >> repository [
	^ repository
]

{ #category : #accessing }
IceWorkingCopy >> repository: anObject [
	repository := anObject
]

{ #category : #'API-properties' }
IceWorkingCopy >> repositoryProperties [
	
	^ properties
]

{ #category : #accessing }
IceWorkingCopy >> rootFileReference [
	
	^ repository location
]

{ #category : #'private-commits' }
IceWorkingCopy >> setMergeStateBetweenCommits: aCollection [ 
	
	self referenceCommit: aCollection
]

{ #category : #'private-notifications' }
IceWorkingCopy >> shouldIgnoreNotifications [
	^ shouldIgnoreNotifications = true
]

{ #category : #'private-monticello' }
IceWorkingCopy >> snapshotFor: package [

	^ (self versionFor: package) snapshot
]

{ #category : #'private-monticello' }
IceWorkingCopy >> snapshotForPackage: aPackage [ 
	
	^ aPackage isLoaded
		ifTrue: [ (MCPackage named: aPackage name) basicSnapshot ]
		ifFalse: [ self referenceCommit snapshotForPackage: aPackage ]
]

{ #category : #'API-loading' }
IceWorkingCopy >> unload [
	
	"Unload all code loaded into the image that belong to this repository"
	self loadedPackages do: [ :each | self unloadPackage: each ]
]

{ #category : #'API-loading' }
IceWorkingCopy >> unloadPackage: aPackage [

	aPackage mcWorkingCopy ifNotNil: #unload
]

{ #category : #'API-loading' }
IceWorkingCopy >> unloadPackageNamed: aPackageName [

	self unloadPackage: (IcePackage named: aPackageName repository: repository)
	
]

{ #category : #validating }
IceWorkingCopy >> validateCanCommit [
	"Verify sync between image version and working copy"	
	"If there are incomming commits is out of sync"
	
	"Check that head is not detached"
	self repository validateCanCommit.
	
	"Check that our current state lets us commit"
	self workingCopyState validateCanCommit
]

{ #category : #'private-monticello' }
IceWorkingCopy >> versionFor: package [

	^ IceSavedPackageVersion fromCommit: self package: package
]

{ #category : #accessing }
IceWorkingCopy >> workingCopyState [
	"The working copy can be in different states depending on the repository and the package. It is the working copy state reponsibility to decide wether we can commit, if we are on a merge, and so on... The working copy state can be obtained through the message #workingCopyState.

  workingCopy workingCopyState.

The working copy state is calculated every time that it is called. This is because the state of the repository can be modified from outside the system (e.g.,  the command line or another tool). In any case, calculating the working copy state is fast enough to be executed on-line even for big repositories such as Pharo's. The working copy state is calculated from the status of each of its packages. It was decided like this because it may happen that somebody downloads a package from different commits. If this situation changes in the future, this is a good point for simplification."

	"This method obtains the head commit once and sends it as argument as an optimization.
	This is because asking for the head commit is expensive.
	
	Check the commits of #packageState"

	referenceCommit isCollection
		ifTrue: [ ^ IceInMergeWorkingCopy repository: repository ].
	referenceCommit isUnknownCommit
		ifTrue: [ ^ IceUnknownVersionWorkingCopy repository: repository ].
	referenceCommit isNoCommit
		ifTrue: [ ^ IceEmptyWorkingCopy repository: repository ].
	^ IceAttachedSingleVersionWorkingCopy repository: repository
]

{ #category : #'API-properties' }
IceWorkingCopy >> writerClass [
	
	^ self referenceCommit writerClass
]
