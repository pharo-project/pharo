"
I am an iceberg repository that uses libgit as backend.
I have a handle to a LGitRepository that is lazily initialized on usage, and cleaned up on every shutdown (automatically done by uFFI).

Every access to the libgit repository should be wrapped with a call to #handleLibgitError: to manage possible libgit errors and transform them to a correct iceberg error.
"
Class {
	#name : #IceLibgitRepository,
	#superclass : #IceRepository,
	#instVars : [
		'location',
		'commitCache',
		'handle'
	],
	#classVars : [
		'DefaultCodeSubdirectory',
		'DefaultFileFormatType',
		'ShareRepositoriesBetweenImages',
		'SharedRepositoriesLocation'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #settings }
IceLibgitRepository class >> defaultCodeSubdirectory [
	^ DefaultCodeSubdirectory ifNil: [ DefaultCodeSubdirectory := '' ]

]

{ #category : #settings }
IceLibgitRepository class >> defaultCodeSubdirectory: aString [
	DefaultCodeSubdirectory := aString

]

{ #category : #accessing }
IceLibgitRepository class >> defaultFileFormat [
	^ Smalltalk at: self defaultFileFormatType asSymbol
]

{ #category : #settings }
IceLibgitRepository class >> defaultFileFormatType [
	^ DefaultFileFormatType ifNil: [ 
		DefaultFileFormatType := TIceRepositoryWriter traitUsers 
			detect: #isDefault
			ifFound: #name
			ifNone: [ nil "To be filled later (on first demand)" ] ]
]

{ #category : #settings }
IceLibgitRepository class >> defaultFileFormatType: anObject [
	DefaultFileFormatType := anObject
]

{ #category : #settings }
IceLibgitRepository class >> formatTypeDomainValues [
	^ TIceRepositoryWriter traitUsers 
		ifNotEmpty: [ :users | users collect: [ :each | each description -> each name ] ]
		ifEmpty: [ { 'Undefined' -> nil } ]
]

{ #category : #rules }
IceLibgitRepository class >> libgitErrorHandlerPragmaSelector [
	
	"Pragma selector used to detect methods used as libgit error handlers.
	This selector is mainly used by architectural rules to ensure that libgit is being called correctly."
	^ #libgitErrorHandler
]

{ #category : #rules }
IceLibgitRepository class >> libgitErrorHandlerSelector [
	
	"Detect what method is used for handling libgit errors.
	Detection is done through a pragma to allow future renames.
	
	This selector is mainly used by architectural rules to ensure that libgit is being called correctly."
	^ (self methods detect: [ :each |
		each pragmas anySatisfy: [ :pragma | pragma keyword = self libgitErrorHandlerPragmaSelector ] ]) selector
]

{ #category : #rules }
IceLibgitRepository class >> libgitRepositoryHandleAccessorPragmaSelector [
	
	"Pragma selector used to detect methods allowed to directly access the repository handle. 
	This selector is mainly used by architectural rules to ensure that libgit is being called correctly."
	^ #repositoryHandleAccessor
]

{ #category : #rules }
IceLibgitRepository class >> libgitRepositoryHandleAccessors [
	
	^ self allMethods select: [ :m | m pragmas anySatisfy: [ :p | p keyword = self libgitRepositoryHandleAccessorPragmaSelector ] ]
]

{ #category : #accessing }
IceLibgitRepository class >> localRepositoriesLocation [
	^ (FileLocator localDirectory / #iceberg) asFileReference
]

{ #category : #accessing }
IceLibgitRepository class >> repositoriesLocation [
 	^ self shareRepositoriesBetweenImages 
		ifTrue: [ self sharedRepositoriesLocation ]
		ifFalse: [ self localRepositoriesLocation ] 
]

{ #category : #utilities }
IceLibgitRepository class >> requiredLibGit2MinimumVersion [
	^ IceSemanticVersion 
		major: 0 
		minor: 25 
		patch: 1
]

{ #category : #settings }
IceLibgitRepository class >> settingsOn: aBuilder [ 
	<systemsettings>
	
	(aBuilder pickOne: #'defaultFileFormatType')
		parent: #Iceberg;
		target: self;
		label: 'File format type';
		description: 'Select your preferred file format.';
		domainValues: self formatTypeDomainValues.
  
	(aBuilder setting: #'defaultCodeSubdirectory')
		parent: #Iceberg;
		target: self;
		label: 'Default Code Subdirectory';
		description: 'Select your preferred source code folder name'.

	(aBuilder setting: #shareRepositoriesBetweenImages)
		parent: #Iceberg;
		target: self;
		label: 'Share repositories between images';
		description: 'Check if you want to have your git repositories cloned by default in a single location that is shared between all your Pharo images';
		with: [
			(aBuilder setting: #sharedRepositoriesLocationString)
				target: self;
				type: #Directory;
				label: 'Location for shared repositories';
				description: 'A directory where you have your git repositories'.
		].
		
]

{ #category : #settings }
IceLibgitRepository class >> shareRepositoriesBetweenImages [
	^ ShareRepositoriesBetweenImages ifNil: [ ShareRepositoriesBetweenImages := false ]
]

{ #category : #settings }
IceLibgitRepository class >> shareRepositoriesBetweenImages: anObject [
	ShareRepositoriesBetweenImages := anObject
]

{ #category : #settings }
IceLibgitRepository class >> sharedRepositoriesLocation [
	^ SharedRepositoriesLocation ifNil: [ 
		SharedRepositoriesLocation := (FileLocator home / #iceberg) asFileReference ]
]

{ #category : #settings }
IceLibgitRepository class >> sharedRepositoriesLocation: anObject [
	SharedRepositoriesLocation := anObject
]

{ #category : #settings }
IceLibgitRepository class >> sharedRepositoriesLocationString [
	^ self sharedRepositoriesLocation fullName
]

{ #category : #settings }
IceLibgitRepository class >> sharedRepositoriesLocationString: aString [
	SharedRepositoriesLocation := aString asFileReference
]

{ #category : #utilities }
IceLibgitRepository class >> verifyBackendAvailabilityIfNot: aBlock [
	| presentVersion requiredVersion |
	
	presentVersion := IceSemanticVersion fromArray: LGitLibrary uniqueInstance version.
	requiredVersion := self requiredLibGit2MinimumVersion.	
	requiredVersion <= presentVersion 
		ifFalse: [ ^ aBlock cull: requiredVersion cull: presentVersion ]
]

{ #category : #'API-files' }
IceLibgitRepository >> addFileToIndex: aFile [
	
	self addFilesToIndex: { aFile }
]

{ #category : #'API-files' }
IceLibgitRepository >> addFilesToIndex: aCollection [

	self handleLibgitError: [
		| gitIndex |
		gitIndex := self repositoryHandle index.
		gitIndex
			addAll:
				(aCollection
					collect: [ :each | 
						self assert: each asFileReference isRelative.
						"self assert: (self location resolve: each) exists."
						each asFileReference path pathString ]
					as: Array).
		gitIndex
			writeToDisk;
			free ]
]

{ #category : #'to-cleanup' }
IceLibgitRepository >> addTo: aMCRepositoryGroup [ 
	"For now do not add me as an MCRepository"
]

{ #category : #'API-branches' }
IceLibgitRepository >> allBranches [
	
	^ self localBranches, self remoteTrackedBranches
]

{ #category : #'API-accessing' }
IceLibgitRepository >> basicCodeDirectory [
	^ self subdirectory isEmptyOrNil 
		ifTrue: [ self repositoryDirectory ]
		ifFalse: [ self repositoryDirectory / self subdirectory  ].
]

{ #category : #'API-branches' }
IceLibgitRepository >> basicNewBranchNamed: aString [ 

	^ IceGitLocalBranch named: aString inRepository: self
]

{ #category : #'API-branches' }
IceLibgitRepository >> basicNewRemoteBranchNamed: aString [ 

	^ IceGitRemoteBranch named: aString inRepository: self
]

{ #category : #'API-branches' }
IceLibgitRepository >> branchNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [

	self handleLibgitError: [ | found |
		found := self repositoryHandle lookupLocalBranch: aString ifAbsent: [ nil ].
		^ found ifNil: absentBlock ifNotNil: [ presentBlock value: (self basicNewBranchNamed: aString) ] ]
]

{ #category : #'private-changes' }
IceLibgitRepository >> changedFilesBetween: aCommitish and: otherCommitish [
	"Answer a list of files that changed between two commits"

	self handleLibgitError: [ | fromTree toTree diff |
		fromTree := (LGitCommit of: self repositoryHandle fromHexString: aCommitish id) tree.
		toTree := (LGitCommit of: self repositoryHandle fromHexString: otherCommitish id) tree.
		diff := fromTree diffTo: toTree.
		^ diff files ]
]

{ #category : #'API-registry' }
IceLibgitRepository >> checkForRegistryConflicts [
	
	"If the repository has no location, do not check for conflicts."
	self location ifNil: [ ^ self ].
	
	self class registry 
		detect: [ :repo | repo location = self location ]
		ifFound: [ :repo | self origin = repo origin
			ifTrue: [ 
				IceDuplicatedRepository signal: 
					('You already have an Iceberg repository 
at {1} 
and with the same origin URL. 

If you really whant create another one, 
please locate it in a different directory.'
					 format: {self location pathString}) ]
			ifFalse: [
				IceDuplicatedRepository signal: 
					('You already have an Iceberg repository at 
{1} 
but with a different origin URL.

You provided {2} 
and the original repository''s url is {3}.'
					 format: {self location. self origin. repo origin}) ]].
]

{ #category : #'API-accessing' }
IceLibgitRepository >> codeDirectory [
	"Code repository might have been deleted or image might have been moved."
	self repositoryDirectory exists
		ifTrue: [ ^ self basicCodeDirectory ]
		ifFalse: [ IceLocalRepositoryMissing signalFor: self ]
]

{ #category : #'private-commits' }
IceLibgitRepository >> commitCache [
	
	^ commitCache ifNil: [ commitCache := LRUCache new maximumWeight: 30  ]
]

{ #category : #'private-commits' }
IceLibgitRepository >> commitFromGitCommit: aLGitCommit [ 
	
	^ self commitCache at: aLGitCommit id hexString ifAbsentPut: [ 
		IceGitCommit new
			id: aLGitCommit id hexString;
			author: aLGitCommit author name asByteArray utf8Decoded;
			datetime: aLGitCommit time asDateAndTime;
			ancestorIds: (aLGitCommit parents collect: [ :parent | parent id hexString ]);
			comment: aLGitCommit message;
			repository: self;
			yourself ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> commitIdFrom: aLGitCommit [ 
	^ aLGitCommit id hexString
]

{ #category : #'API-accessing' }
IceLibgitRepository >> commitishNamed: aName [
	"Resolve a name in the form of:
	  - a commit hash
	  - a branch name
	  - some other expression such as HEAD or master~1
	and return the corresponding iceberg object for it."

	^ self
		branchNamed: aName
		ifAbsent: [ "If there is a single remote, we can look for the branch in this single remote."
			self remotes size = 1
				ifTrue: [ self remotes anyOne
						remoteBranchNamed: aName
						ifPresent: [ :found | ^ found ]
						ifAbsent: [ "Continue with the revparse" ] ].
			self revparse: aName ]
]

{ #category : #'API-branches' }
IceLibgitRepository >> createBranch: branchName [
	| newBranch |
	"This is really low level and may provoke problems.
	Libgit does not provide an API to change branch when the repository is unborn.
	Instead, we replace the HEAD file adding a reference with the new branch.
			
	This may break other existing refs in the repository."
	self handleLibgitError: [ [ self repositoryHandle isUnborn
				ifTrue: [ (self location / '.git' / 'HEAD')
						ensureDelete;
						writeStreamDo: [ :stream | 
							stream
								nextPutAll: 'ref: refs/heads/';
								nextPutAll: branchName ] ]
				ifFalse: [ newBranch := self repositoryHandle createBranch: branchName target: self repositoryHandle head.
					self repositoryHandle setHead: newBranch ] ]
				on: LGit_GIT_EEXISTS
				do: [ :error | 
					IceBranchAlreadyExists new
						branchName: branchName;
						signal ].
			^ self head ]
]

{ #category : #'API-branches' }
IceLibgitRepository >> createBranch: aString inCommit: anIceGitCommit [

	self handleLibgitError: [
		(self repositoryHandle isUnborn and: [ anIceGitCommit isNoCommit ])
			ifTrue: [ ^ self createBranch: aString ].
		self repositoryHandle createBranch: aString targetId: (LGitId fromHexString: anIceGitCommit gitRef) force: false.
		^ (self branchNamed: aString)
			switch;
			yourself ]
]

{ #category : #'API-working copy' }
IceLibgitRepository >> createSourceDirectory [
	(location / self subdirectory) ensureCreateDirectory.
	
	self properties
		fileFormat: self class defaultFileFormat;
		store	
]

{ #category : #'API-registry' }
IceLibgitRepository >> delete [
	"Delete is idempotent. If the repository does not exist, do nothing.
	Otherwise, forget the repository and remove it from the disk.
	Cleanup also the FFI handles."
	
	super delete.
	
	handle := nil.
	self locationIfPresent: [ :aLocation | aLocation ensureDeleteAll ]
]

{ #category : #printing }
IceLibgitRepository >> description [
	^ [ super description ] on: Error do: [ 
		self location isNil
			ifTrue: [ 'Not specified location' ]
			ifFalse: [ 'Bad location: ', self location path pathString ] ]
]

{ #category : #'API-packages' }
IceLibgitRepository >> directoryForPackage: mcPackage [
	| package |
	package := self packageNamed: mcPackage name.
	^ self subdirectory isEmptyOrNil
		ifTrue: [ self directoryNameForPackage: package ]
		ifFalse: [ self subdirectory, '/', (self directoryNameForPackage: package) ]
]

{ #category : #'API-packages' }
IceLibgitRepository >> directoryNameForPackage: package [

	^ (self writerClass 
		directoryNameFor: package)
		asString
]

{ #category : #'API-packages' }
IceLibgitRepository >> directoryPathStringForPackage: aMCPackage [ 

	^ self subdirectory isEmptyOrNil 
		ifTrue: [ self directoryNameForPackage: aMCPackage ]
		ifFalse: [ self subdirectory, '/',  (self directoryNameForPackage: aMCPackage) ]
]

{ #category : #'private-remotes' }
IceLibgitRepository >> doRemoveRemote: aRemote [
	self handleLibgitError: [
		(LGitRemote of: self repositoryHandle named: aRemote name)
			lookup;
			delete ]
]

{ #category : #'private-libgit' }
IceLibgitRepository >> free [

	(handle isNil or: [ handle isNull ])
		ifFalse: [ 
			handle free.
			handle := nil ]
]

{ #category : #'API-configuration' }
IceLibgitRepository >> getConfigurationGlobal: aBoolean [

	self handleLibgitError: [ | config |
		config := self repositoryHandle config.
		^ aBoolean
			ifTrue: [ config openGlobal ]
			ifFalse: [ config ] ]
]

{ #category : #'private-libgit' }
IceLibgitRepository >> handleLibgitError: aBlock [
	"Handle errors while calling libgit.
	The pragma is used to detect this method and verify on architectural rules that it is correctly called"
	<libgitErrorHandler>
	
	aBlock on: LGitCallReturnHandler do: [ :error | error acceptError: (IceLibgitErrorVisitor onContext: self) ]
]

{ #category : #'API-tags' }
IceLibgitRepository >> hasTagNamed: aString [ 
	
	^ self tagNames includes: aString
]

{ #category : #'API-accessing' }
IceLibgitRepository >> head [
	
	^ self lookupHead
]

{ #category : #printing }
IceLibgitRepository >> headDescription [
	
	^ [ super headDescription ] on: Error do: [ 'Bad location' ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> iceCommitFrom: aGitCommit [
	"The type of 'aCommitish' depends on how each backend internal representation. "

	^ self commitFromGitCommit: aGitCommit
]

{ #category : #'private-initialization' }
IceLibgitRepository >> init [
	
	"Initialize new repository. This is the equivalent to running in the command line:
	  $ git init
	"
	(LGitRepository on: self location) init
]

{ #category : #'private-commits' }
IceLibgitRepository >> internalStoreVersion: aMCVersion [
	self writerClass
		forInternalStoreFileOut: aMCVersion 
		on: self
]

{ #category : #testing }
IceLibgitRepository >> isCloneOfUrl: aString [

	^ self isMissing not and: [ super isCloneOfUrl: aString ]
]

{ #category : #testing }
IceLibgitRepository >> isCodeMissing [
	^ self subdirectory isEmptyOrNil 
		ifTrue: [ self isMissing ]
		ifFalse: [ (self repositoryDirectory / self subdirectory) exists not  ]
]

{ #category : #testing }
IceLibgitRepository >> isMissing [
	^ self repositoryDirectory isNil 
		or: [ self repositoryDirectory exists not ]
]

{ #category : #'API-packages' }
IceLibgitRepository >> isModified [
	"We cannot correctly determine if a repository is modified if it is missing so, false"
	
	^ self isMissing not and: [ super isModified ]
]

{ #category : #'API-packages' }
IceLibgitRepository >> isPackageExported: anIceSavedPackage [

	| packageLocation |
	packageLocation := self packageLocationFor: anIceSavedPackage.
	^ packageLocation exists and: [ packageLocation hasChildren ]
]

{ #category : #testing }
IceLibgitRepository >> isValid [
	self flag: #todo. "Make this tests more solid"
	^ self isMissing not
]

{ #category : #'API-branches' }
IceLibgitRepository >> localBranches [

	self handleLibgitError: [ 
		self repositoryHandle isUnborn
			ifTrue: [ ^ {self head} ].
		^ OrderedCollection
			new: 10
			streamContents: [ :stream | self repositoryHandle localBranchesDo: [ :br :isLocal | stream nextPut: (self basicNewBranchNamed: br name) ] ] ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> location [
	^ location
]

{ #category : #'API-accessing' }
IceLibgitRepository >> location: aFileReference [
	location := aFileReference.
	self resetHandle
]

{ #category : #'API-accessing' }
IceLibgitRepository >> locationIfPresent: aBlock [

	^ self locationIfPresent: aBlock ifAbsent: [ self ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> locationIfPresent: aBlock ifAbsent: absentBlock [

	self location ifNil: [ ^ absentBlock value ].

	^ self location exists
		ifTrue: [ aBlock value: location ]
		ifFalse: [ absentBlock value ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> log [
	^ self logClass for: self
]

{ #category : #'API-accessing' }
IceLibgitRepository >> logClass [
	^ IceLog allSubclasses 
		detect: [ :each | each isAvailableFor: self ]
]

{ #category : #'API-commits' }
IceLibgitRepository >> lookupCommit: commitId ifAbsent: aBlock [
	^ [ self revparse: commitId ]
		on: NotFound
		do: aBlock
]

{ #category : #'API-accessing' }
IceLibgitRepository >> lookupHead [
	self handleLibgitError: [ | head |
		self repositoryHandle isUnborn
			ifTrue: [ ^ IceGitUnbornBranch inRepository: self ].
		head := self repositoryHandle head.
		^ head isBranch
			ifTrue: [ self basicNewBranchNamed: head name ]
			ifFalse: [ head object asIcebergObjectInRepository: self ] ]
]

{ #category : #'private-monticello' }
IceLibgitRepository >> mcVersionFor: iceVersion [
	[ | commit |
		commit := iceVersion commit libgitCommit.
		^ (iceVersion commit readerClass on: commit iceVersion: iceVersion) version ]
	on: IceMissingRepositoryEntry, LGit_GIT_ENOTFOUND
	do: [ 
		IceVersionDoesNotExist new
			version: iceVersion;
			signal ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> mergeBaseBetween: aCommitId and: anotherCommitId [
	
	self handleLibgitError: [ ^ (self repositoryHandle
			mergeBaseBetween: (LGitId fromHexString: aCommitId)
			and: (LGitId fromHexString: anotherCommitId)) hexString ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> name [

	^ name ifNil: [ name := self obtainRepositoryName ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> newCommitWalk [
	^ IceLibgitCommitWalk forRepository: self
]

{ #category : #'private-changes' }
IceLibgitRepository >> newIndex [
	
	^ IceGitIndex on: self
]

{ #category : #'API-commits' }
IceLibgitRepository >> newNoCommit [
	
	^ IceNoCommit repository: self
]

{ #category : #'API-project' }
IceLibgitRepository >> newProjectFromCommit: aCommit [

	^ IceProjectReader readProjectFrom: self
]

{ #category : #'API-accessing' }
IceLibgitRepository >> obtainRepositoryName [
	
	^ self location basename
]

{ #category : #'API-remotes' }
IceLibgitRepository >> origin [
	
	^ self
		remoteNamed: 'origin'
		ifAbsent: [
			IceUndefinedRemote new
				name: 'origin';
				yourself ]
]

{ #category : #'API-packages' }
IceLibgitRepository >> packageLocationFor: anIceSavedPackage [
	| packageLocation directoryName |
	 
	packageLocation := self location.
	self subdirectory ifNotEmpty: [ 
		packageLocation := packageLocation / self subdirectory ].
	directoryName := self writerClass directoryNameFor: anIceSavedPackage.
	packageLocation := packageLocation / directoryName.
	^ packageLocation
]

{ #category : #'private-tags' }
IceLibgitRepository >> peelTag: anIceTag [ 
	self handleLibgitError: [ | lgitRef |
		lgitRef := (self repositoryHandle lookup: 'refs/tags/', anIceTag name).
		^ self lookupCommit: lgitRef targetId hexString ]
]

{ #category : #'API-project' }
IceLibgitRepository >> project [
	
	^ self workingCopy project
]

{ #category : #'API-accessing' }
IceLibgitRepository >> properties [
	^ self workingCopy repositoryProperties
]

{ #category : #'API-properties' }
IceLibgitRepository >> readerClass [
	^ self properties readerClass
]

{ #category : #'API-remotes' }
IceLibgitRepository >> remoteAt: aString put: aRemote [

	self handleLibgitError: [ self repositoryHandle addRemote: aRemote name url: aRemote url ]
]

{ #category : #'API-remotes' }
IceLibgitRepository >> remoteBranchNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [
	
	self handleLibgitError: [ | branchRef |
		branchRef := self repositoryHandle lookupRemoteBranch: aString ifAbsent: [ ^ absentBlock value ].
		presentBlock value: (self basicNewRemoteBranchNamed: branchRef name) ]
]

{ #category : #'API-remotes' }
IceLibgitRepository >> remoteNamed: aName ifAbsent: aBlockClosure [
	
	self handleLibgitError: [ | gitRemote |
	[ gitRemote := (LGitRemote of: self repositoryHandle named: aName) lookup.
	^ (IceGitRemote name: gitRemote remoteName url: gitRemote url)
		localRepository: self;
		yourself ]
		on: LGitObjectNotInitialized
		do: [ ^ aBlockClosure value ] ]
]

{ #category : #'API-remotes' }
IceLibgitRepository >> remoteTrackedBranches [
	
	self handleLibgitError: [
		^ OrderedCollection
			new: 10
			streamContents: [ :stream | 
				self repositoryHandle
					remoteBranchesDo: [ :br :isLocal | 
						self assert: isLocal not.
						stream nextPut: (self basicNewRemoteBranchNamed: br name) ] ] ]
]

{ #category : #'API-remotes' }
IceLibgitRepository >> remotes [
	
	self handleLibgitError: [ | gitRemotes |
		self isValid ifFalse: [ ^ #() ].
		gitRemotes := self repositoryHandle allRemotes.
		^ gitRemotes
			collect: [ :each | 
				(IceGitRemote name: each remoteName url: each url)
					localRepository: self;
					yourself ] ]
]

{ #category : #'API-tags' }
IceLibgitRepository >> removeTag: anIceTag [ 
	
	self handleLibgitError: [
		self repositoryHandle git_tag_delete: anIceTag name ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> repositoryDirectory [
	^ self location
]

{ #category : #'private-libgit' }
IceLibgitRepository >> repositoryHandle [

	<repositoryHandleAccessor>
	(handle isNil or: [ handle isNull ])
		ifTrue: [ handle := (LGitRepository on: self location) open ].
	^ handle
]

{ #category : #'private-libgit' }
IceLibgitRepository >> resetHandle [
	handle := nil
]

{ #category : #'API-commits' }
IceLibgitRepository >> revparse: aName [

	self handleLibgitError: [ | gitCommit gitId |
		gitId := (self repositoryHandle revparse: aName) object id.
		gitCommit := LGitCommit of: self repositoryHandle fromId: gitId.
		^ self commitFromGitCommit: gitCommit ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> setHead: anIceGitCommitish [
	
	"Need to keep the reference, if not it is garbage collected. As the options are only passed to a FFI call"
	| progress |
	progress := IceGitCheckoutProgress new.
	
	self handleLibgitError: [ 
		self repositoryHandle
			checkout: anIceGitCommitish gitRef
			options:
				(LGitCheckoutOptions defaults
					checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_force;
					progressCallback: progress;
					yourself) ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> subdirectory [
	^ self project sourceDirectory
]

{ #category : #'API-accessing' }
IceLibgitRepository >> subdirectory: aString [ 
	
	self project sourceDirectory: aString
]

{ #category : #'API-accessing' }
IceLibgitRepository >> subdirectoryPath [
	
	"API compatible both with Pharo6 and Pharo7.
	This could be replaced by the Pharo7 API
	
	RelativePath with: self subdirectory
	"
	^ RelativePath withAll: (RelativePath canonicalizeElements: ($/ split: self subdirectory))
]

{ #category : #'API-accessing' }
IceLibgitRepository >> subdirectoryReference [
	
	^ self location resolve: self subdirectoryPath
]

{ #category : #'API-tags' }
IceLibgitRepository >> tags [
	self handleLibgitError: [ 
		^ self repositoryHandle tags
			collect: [ :each | 
				IceTag named: each inRepository: self ] ]
]

{ #category : #'API-accessing' }
IceLibgitRepository >> writerClass [
	^ self project writerClass
]
