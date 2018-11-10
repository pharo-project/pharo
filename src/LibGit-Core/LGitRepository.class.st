"
A LGitRepository represents a git repository.

Instance Variables
	handle:		NBExternalObject

handle
	- reference to the external object

"
Class {
	#name : #LGitRepository,
	#superclass : #LGitExternalObject,
	#instVars : [
		'repositoryPath',
		'isOpen',
		'workingDirectory'
	],
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'native-boost' }
LGitRepository class >> freeFunctionSelector [
	^ #repository_free:
]

{ #category : #'instance creation' }
LGitRepository class >> new [
	self error: 'This class should be instantiated with a give path.'
]

{ #category : #'instance creation' }
LGitRepository class >> on: aFileReference [
	^ self basicNew
		initialize;
		initializeOn: aFileReference
]

{ #category : #'libgit-calls' }
LGitRepository class >> repository_free: handle [
	
	^ self call: #(void git_repository_free #(void *handle)) options: #()
]

{ #category : #accessing }
LGitRepository >> addRemote: aName url: anUrl [
	(LGitRemote of: self named: aName url: anUrl) createWithoutSpec
]

{ #category : #accessing }
LGitRepository >> allBranches [
	^ OrderedCollection new: 10 streamContents: [ :stream |
		self allBranchesDo: [ :reference | 
			stream nextPut: reference ] ]
]

{ #category : #enumerating }
LGitRepository >> allBranchesDo: aBlock [
	LGitBranchIterator allBranchesOf: self do: aBlock
]

{ #category : #accessing }
LGitRepository >> allRemotes [
	| out |
	
	out := LGitStringArray externalNew autoRelease.
	self remote_list: out.
	^ out strings collect: [ :each | (LGitRemote of: self named: each) lookup ]
]

{ #category : #accessing }
LGitRepository >> beOpen [
	isOpen := true
]

{ #category : #accessing }
LGitRepository >> branchNames [
	^ self branches collect: #name
]

{ #category : #'libgit-calls' }
LGitRepository >> branch_lookup: out branch_name: branch_name branch_type: branch_type [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_branch_lookup #(LGitReference * out , self , String branch_name , LGitBranchTypeEnum branch_type))
		options: #()
]

{ #category : #accessing }
LGitRepository >> branches [
	^ OrderedCollection new: 10 streamContents: [ :stream |
		self branchesDo: [ :reference | stream nextPut: reference ] ]
]

{ #category : #enumerating }
LGitRepository >> branchesDo: aBlock [
	LGitBranchIterator localBranchesOf: self do: aBlock.

]

{ #category : #operations }
LGitRepository >> checkout: aString [
	
	self checkout: aString options: (LGitCheckoutOptions defaults
		checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_safe;
		yourself)
]

{ #category : #operations }
LGitRepository >> checkout: aString options: options [
	<todo: 'use proper exceptions'>
	self assert: aString notNil.
	self assert: aString trimBoth notEmpty.
	[ 
		| branch |
		branch := self 
			lookupLocalBranch: aString 
			ifAbsent: [ self createBranchFromOrigin: aString ].
		self setHead: branch.
		self resetHardTo: branch object checkoutOptions: options
	] 
	on: LGit_GIT_EINVALIDSPEC, LGit_GIT_ENOTFOUND do: [
		"aString is not a valid branch name, so try to find a corresponding treeish using revparse, 
		this will handle stuff like 'master~1'"
		| commitish | 
		commitish := self revparse: aString.
		self setHeadDetached: commitish.
		self resetHardTo: commitish object checkoutOptions: options.
	].


]

{ #category : #operations }
LGitRepository >> checkoutHead [
	self withReturnHandlerDo: [ 
		self checkout_head_opts: (
			LGitCheckoutOptions defaults
				checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_safe;
				yourself) ]

]

{ #category : #'libgit-calls' }
LGitRepository >> checkout_head_opts: opts [
	^ self
		call:
			#(LGitReturnCodeEnum git_checkout_head #(self , LGitCheckoutOptions * opts))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> checkout_treeish: treeish [
	"treeish can be a commit, tag or tree (or NULL to use HEAD)"
	self withReturnHandlerDo: [ 
		self
			checkout_treeish: treeish
			opts:
				(LGitCheckoutOptions defaults
					checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_safe;
					yourself) ].

]

{ #category : #'libgit-calls' }
LGitRepository >> checkout_treeish: treeish opts: opts [
	^ self
		call:
			#(LGitReturnCodeEnum git_checkout_tree #(self , LGitObject treeish , LGitCheckoutOptions * opts))
		options: #()
]

{ #category : #operations }
LGitRepository >> clone: aString [
	| cloneOptions |
	cloneOptions := LGitCloneOptions defaults.
	self
		clone: aString
		options: cloneOptions	

]

{ #category : #'libgit-calls' }
LGitRepository >> clone: aString options: cloneOptions [
	self
		clone: aString
		options: cloneOptions
		to: self repositoryReference
]

{ #category : #'libgit-calls' }
LGitRepository >> clone: aString options: cloneOptions to: aFileReference [
	<todo: 'use exceptions instead of assertion'>
	self assert: aString isEmptyOrNil not.
	self assert: aFileReference notNil.
	self assert: cloneOptions notNil.
	
	aFileReference ensureCreateDirectory.
	
	self withReturnHandlerDo: [ 
		self 
			clone: self
			url: aString
			local_path: aFileReference pathString
			options: cloneOptions ]
]

{ #category : #'libgit-calls' }
LGitRepository >> clone: out url: url local_path: local_path options: options [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_clone (LGitRepository *out, String url, String local_path, LGitCloneOptions *options))
		options: #(optCoerceNilToNull)
]

{ #category : #operations }
LGitRepository >> clone: aString withCredentials: aCredentialsProvider [ 
	| cloneOptions |
	cloneOptions := LGitCloneOptions withCredentialsProvider: aCredentialsProvider.
	self
		clone: aString
		options: cloneOptions	

]

{ #category : #operations }
LGitRepository >> cloneHttps: aString [
	<todo: 'use exceptions instead of assertion'>
	self assert: (aString beginsWith: 'https://').
	
	self clone: aString
]

{ #category : #operations }
LGitRepository >> cloneLocal: aString [
	self clone: aString
]

{ #category : #operations }
LGitRepository >> cloneRemote: aRemote signature: aSignature [
	self
		cloneRemote: aRemote
		signature: aSignature
		branch: 'master'
]

{ #category : #operations }
LGitRepository >> cloneRemote: aRemote signature: aSignature branch: aString [
	self
		cloneRemote: aRemote
		signature: aSignature
		branch: aString
		checkoutOptions: LGitCheckoutOptions defaults
]

{ #category : #operations }
LGitRepository >> cloneRemote: aRemote  signature: aSignature branch: aString checkoutOptions: options [
	<todo: 'use exceptions instead of assertions'>
	self assert: aRemote hasFetchSpec.
	
	self withReturnHandlerDo: [
		self 
			clone_into: self
			remote: aRemote
			co_opts: options
			branch: aString
			signature: aSignature ]
]

{ #category : #operations }
LGitRepository >> cloneSsh: aString [
	| credentials cloneOptions |
	credentials := LGitRemote credentialsSsh.
	cloneOptions := LGitCloneOptions withCredentialsCallback: 
		[ :cred :url :username_from_url :allowed_types :data | 
			allowed_types = 32 
				ifTrue: [ credentials setGitUsernameIn: cred ]
				ifFalse: [ credentials createWith: cred ].
			0 
		].
	
	self
		clone: aString
		options: cloneOptions	
]

{ #category : #accessing }
LGitRepository >> config [
	| config |
	config := (LGitConfig of: self) autoRelease.
	self withReturnHandlerDo: [ self repository_config: config ].
	^ config

]

{ #category : #operations }
LGitRepository >> create [
	self exists ifTrue: [ ^ self ].
	
	self repositoryReference ensureCreateDirectory
]

{ #category : #operations }
LGitRepository >> createBranch: aString force: aBoolean [
	<todo: 'this method belongs somewhere else and should allow to specify the target'>
	| reference |
	reference := LGitReference of: self.
	reference createSymbolic: 'refs/heads/' , aString target: 'refs/dummy' force: aBoolean.
	^ reference
]

{ #category : #operations }
LGitRepository >> createBranch: aString target: aGitReference [
	<todo: 'this method belongs somewhere else'>
	^ self createBranch: aString target: aGitReference force: false

]

{ #category : #operations }
LGitRepository >> createBranch: aString target: aGitReference force: aBoolean [
	<todo: 'this method belongs somewhere else'>
	| reference |
	reference := LGitReference of: self.
	reference 
		createDirect: 'refs/heads/' , aString 
		target: aGitReference object id 
		force: aBoolean.
	^ reference
]

{ #category : #operations }
LGitRepository >> createBranch: aString targetId: aGitId force: aBoolean [
	<todo: 'this method belongs somewhere else'>
	| reference |
	reference := LGitReference of: self.
	reference createDirect: 'refs/heads/' , aString target: aGitId force: aBoolean.
	^ reference
]

{ #category : #operations }
LGitRepository >> createBranchFromOrigin: branchName [
	| remoteBranchName remoteBranch localBranch |
	remoteBranchName := 'origin/', branchName.
	remoteBranch := self lookupRemoteBranch: remoteBranchName.
	localBranch := self createBranch: branchName target: remoteBranch.
	localBranch setUpstream: remoteBranchName.
	^ localBranch
]

{ #category : #private }
LGitRepository >> createPackForReference: aReference [
	| odb walk packbuilder writepack stats packbuilderPayload progresscb packbuilderCallback |
	odb := self database.
	walk := (LGitRevwalk of: self)
		beSortedByCommitTime;
		pushReference: aReference;
		yourself.
	packbuilder := LGitPackBuilder new.
	packbuilder packbuilder_new: packbuilder repo: self.	"hideCallback := LG2RevwalkHideCb on: [ :oid :payload |  ].
	LG2RevwalkH uniqueInstance
		revwalk_add_hide_cb: walk
		hide_cb: hideCallback
		payload: nil."
	[ walk hasNext ] whileTrue: [ 
		| commit tree |
		commit := walk next.
		tree := commit tree.
		packbuilder
			packbuilder_insert_commit: packbuilder id: commit id;
			packbuilder_insert_tree: packbuilder id: tree id.
		commit free.
		tree free ].
	progresscb := LGitTransferProgressCallback on: [ :progress :data | 0 ].
	writepack := LGitOdbWritepack externalNew.
	writepack
		odb_write_pack: writepack
		db: odb
		progress_cb: progresscb
		progress_payload: ExternalAddress null.
	writepack := LGitOdbWritepack fromHandle: writepack getHandle pointer.
	stats := LGitTransferProgress externalNew.
	packbuilderPayload := LGitPackbuilderForeachPayload externalNew.
	packbuilderCallback := LGitPackbuilderForeachCallback
		on: [ :buffer :length :payload | payload writepack appendWithBuffer: buffer length: length progress: payload stats ].
	packbuilderPayload
		writepack: writepack;
		stats: stats.
	packbuilder packbuilder_foreach: packbuilder cb: packbuilderCallback payload: packbuilderPayload.
	writepack commitWithProgress: stats
]

{ #category : #operations }
LGitRepository >> createTag: aString targetId: aGitId force: aBoolean [
	"Create a tag. Based on #createBranch:targetId:force:"

	| reference |
	reference := LGitReference of: self.
	reference
		createDirect: 'refs/tags/', aString
		target: aGitId
		force: aBoolean.
	^ reference
]

{ #category : #operations }
LGitRepository >> database [
	| database |
	database := LGitOdb new.
	self withReturnHandlerDo: [
		self
			repository_odb: database
			repo: self ].
		
	^ database
]

{ #category : #operations }
LGitRepository >> defaultSignature [
	| signaturePointer |
	signaturePointer := ExternalAddress new.
	self withReturnHandlerDo: [ self signature_default: signaturePointer repo: self ].
	^ LGitSignature fromHandle: signaturePointer
]

{ #category : #private }
LGitRepository >> deleteAllLooseObjectsForReference: aReference [
	(self repositoryReference / '.git' / 'objects') children
		select: [ :entry | entry basename ~= 'pack' ]
		thenDo: [ :entry | entry deleteAll ].
	
	"raise an exception if not resolvable"
	self head
	
"	| walker oid baseReference |
	walker := LGitRevwalkType new.
	LG2RevwalkH uniqueInstance
		revwalk_new: walker
		repo: self.
	LG2RevwalkH uniqueInstance
		revwalk_sorting: walker
		sort_mode: 2.
	LG2RevwalkH uniqueInstance
		revwalk_push_ref: walker
		refname: aReference name.

	baseReference := aReference repository repositoryReference / '.git' / 'objects'.
	oid := LGitId new.
	[ (LG2RevwalkH uniqueInstance
		revwalk_next: oid
		walk: walker) isZero ] whileTrue: [
			| fileReference |
			fileReference := baseReference resolve: oid asRelativePathString.
			fileReference delete.
			fileReference parent hasChildren ifFalse: [ fileReference parent delete ] ]"
]

{ #category : #operations }
LGitRepository >> deleteBranch: aGitReference [

	(self git_branch_delete: aGitReference) handleLGitReturnCode
]

{ #category : #testing }
LGitRepository >> exists [
	^ self repositoryReference exists
]

{ #category : #operations }
LGitRepository >> fastForward [
	<todo: 'check for conflicts'>
	"currently forces fastForward in every case"
	self head resolve setTarget: (self lookup: 'FETCH_HEAD')
]

{ #category : #operations }
LGitRepository >> fetchFrom: remoteName [ 
	(LGitRemote of: self named: remoteName) 
		lookup; 
		fetch
]

{ #category : #'libgit-calls' }
LGitRepository >> git_branch_delete: gitReference [
	^ self
		call:
			#(LGitReturnCodeEnum git_branch_delete #(LGitReference gitReference))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> git_tag_delete: aTagName [
	"int git_tag_delete(git_repository *repo, const char *tag_name);"
	^ self
		call: #(LGitReturnCodeEnum git_tag_delete(self , String aTagName))
		options: #()
]

{ #category : #accessing }
LGitRepository >> head [
	| head |
	head := LGitReference of: self.
	self withReturnHandlerDo: [ self repository_head: head of: self ].
	^ head
]

{ #category : #accessing }
LGitRepository >> index [
	| index |
	index := LGitIndex of: self.
	self withReturnHandlerDo: [ self repository_index: index repo: self ].
	^ index
]

{ #category : #operations }
LGitRepository >> init [
	self initBare: false
]

{ #category : #operations }
LGitRepository >> initBare: aBoolean [
	self withReturnHandlerDo: [
		self
			repository_init: self
			path: self repositoryReference pathString
			is_bare: aBoolean ].
	self autoRelease 
]

{ #category : #initialization }
LGitRepository >> initializeOn: aFileReference [
	repositoryPath := aFileReference
]

{ #category : #testing }
LGitRepository >> isBare [
	^ self repository_is_bare: self
]

{ #category : #testing }
LGitRepository >> isDirty: aFileReference [
	^ self index isDirty: aFileReference
]

{ #category : #testing }
LGitRepository >> isEmpty [
	self exists ifFalse: [ ^ true ].
	
	^ self repository_is_empty: self
]

{ #category : #testing }
LGitRepository >> isInitialized [
	^ self isReady
]

{ #category : #testing }
LGitRepository >> isOpen [
	^ isOpen ifNil: [ false ]
]

{ #category : #testing }
LGitRepository >> isProperRepository [
	| return |
	return := self
		repository_open_ext: self
		path: self repositoryReference pathString
		flags: LGitOpenFlagEnum git_repository_open_no_search
		ceiling_dirs: nil.
		
	^ return isOk
]

{ #category : #testing }
LGitRepository >> isUnborn [
	^ self repository_is_unborn: self
]

{ #category : #enumerating }
LGitRepository >> localBranchesDo: aBlock [
	LGitBranchIterator localBranchesOf: self do: aBlock
]

{ #category : #lookup }
LGitRepository >> lookup: name [
	| reference |
	reference := LGitReference of: self.
	self withReturnHandlerDo: [
		self
			reference_lookup: reference
			repo: self
			shorthand: name ].
		
	^ reference
]

{ #category : #lookup }
LGitRepository >> lookupBranch: name [
	"DO NOT USE: This should be deprecated, but causes a lot of problems."
	"self deprecated: 'Use #lookupBranch:isRemote: instead'."
	self flag: #todo. "Remove this method."
	^ (name includes: $/)
		ifTrue: [ self lookupRemoteBranch: (name withoutPrefix: 'refs/remotes/') ]
		ifFalse: [ self lookupLocalBranch: name ] 

]

{ #category : #lookup }
LGitRepository >> lookupBranch: branchName inRemote: remoteName [
	^ self lookupRemoteBranch: remoteName, '/', branchName
]

{ #category : #lookup }
LGitRepository >> lookupBranch: name remote: isRemote [
	^ isRemote
		ifTrue: [ self lookupRemoteBranch: (name withoutPrefix: 'refs/remotes/') ]
		ifFalse: [ self lookupLocalBranch: name ]
]

{ #category : #lookup }
LGitRepository >> lookupBranch: name type: branchType [
	| reference |
	reference := LGitReference of: self.
	self withReturnHandlerDo: [ 
		self
			branch_lookup: reference
			branch_name: name
			branch_type: branchType
	].
	^ reference

]

{ #category : #lookup }
LGitRepository >> lookupLocalBranch: name [
	^ self lookupBranch: name type: LGitBranchTypeEnum git_branch_local 
]

{ #category : #lookup }
LGitRepository >> lookupLocalBranch: name ifAbsent: aBlock [
	^ [ self lookupLocalBranch: name ]
		on: LGit_GIT_ENOTFOUND 
		do: aBlock 
]

{ #category : #lookup }
LGitRepository >> lookupRemoteBranch: name [
	^ self lookupBranch: name type: LGitBranchTypeEnum git_branch_remote
]

{ #category : #lookup }
LGitRepository >> lookupRemoteBranch: name ifAbsent: aBlock [
	^ [ self lookupRemoteBranch: name ]
		on: LGit_GIT_ENOTFOUND 
		do: aBlock 
]

{ #category : #operations }
LGitRepository >> merge: aLGitAnnotatedCommit [ 
	| heads |
	heads := (FFIExternalArray externalNewType: LGitAnnotatedCommit size: 1) autoRelease;
		at: 1 put: aLGitAnnotatedCommit handle;
		yourself.

	self withReturnHandlerDo: [
		self merge_heads: heads 
			length: 1 
			merge_options: LGitMergeOptions defaults
			checkout_options: LGitCheckoutOptions defaults 
	]

]

{ #category : #operations }
LGitRepository >> mergeBaseBetween: aLGitId and: aLGitId2 [ 
	| base |
	base := LGitId new.
	self withReturnHandlerDo: [ self merge_base: base between: aLGitId and: aLGitId2 ].
	^ base
]

{ #category : #'libgit-calls' }
LGitRepository >> merge_base: out between: one and: two [
	^ self call: #(	LGitReturnCodeEnum git_merge_base #(
			LGitId * out, 
			self, 
			LGitId * one, 
			LGitId * two))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> merge_heads: heads length: length merge_options: mergeOptions checkout_options: checkoutOptions [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_merge #(
				self , 
				FFIExternalArray heads ,
				int length , 
				LGitMergeOptions * mergeOptions ,
				LGitCheckoutOptions * checkoutOptions))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> new_signature: out name:name email:email time:time [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_signature_new #(void **out , String name, String email, LGitTime time, 0))
		options: #()
]

{ #category : #printing }
LGitRepository >> objectPrintOn: aStream [
	aStream nextPutAll: self repositoryReference fullName.
	self isBare 
		ifTrue: [ aStream nextPutAll: ' [bare]' ]
]

{ #category : #operations }
LGitRepository >> open [
	self withReturnHandlerDo: [
		self
			repository_open: self
			bare_path: self repositoryReference pathString ].
	self beOpen 
]

{ #category : #accessing }
LGitRepository >> origin [
	^ (LGitRemote of: self named: 'origin') lookup; yourself
]

{ #category : #operations }
LGitRepository >> packHead [
	<todo: 'This method is distructive. It will delete *all* objects, including those not in the pack (not reachable from head'>
	"USE VERY CAREFULLY"
	
	self createPackForReference: self head.
	self deleteAllLooseObjectsForReference: self head
]

{ #category : #'libgit-calls' }
LGitRepository >> reference_lookup: out repo: repo shorthand: shorthand [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_reference_lookup #(LGitReference * out , self , String shorthand))
		options: #()
]

{ #category : #enumerating }
LGitRepository >> remoteBranchesDo: aBlock [
	LGitBranchIterator remoteBranchesOf: self do: aBlock
]

{ #category : #'libgit-calls' }
LGitRepository >> remote_list: out [
	^ self 
		call: #( int git_remote_list ( LGitStringArray *out, self ) ) 
		options: #()
]

{ #category : #accessing }
LGitRepository >> repositoryReference [
	^ repositoryPath
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_config: config [
	^ self
		call:
			#(LGitReturnCodeEnum git_repository_config(LGitConfig *config, self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_head: reference of: repository [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_repository_head #(LGitReference * reference , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_index: out repo: repo [
	
	^ self
		call: #(LGitReturnCodeEnum git_repository_index #(LGitIndex * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_init: out path: path is_bare: is_bare [
	
	^ self
		callUnchecked:
			#(int git_repository_init #(LGitRepository * out , String path , Boolean is_bare))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_is_bare: repo [
	
	^ self call: #(Boolean git_repository_is_bare #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_is_empty: repo [
	
	^ self call: #(Boolean git_repository_is_empty #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_is_unborn: repo [
	^ self call: #( Boolean git_repository_head_unborn (self) ) options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_odb: out repo: repo [
	
	^ self
		call: #(LGitReturnCodeEnum git_repository_odb #(LGitOdb * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_open: out bare_path: bare_path [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_repository_open #(LGitRepository * out , String bare_path))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_open_ext: out path: path flags: flags ceiling_dirs: ceiling_dirs [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_repository_open_ext #(LGitRepository * out , String path , LGitOpenFlagEnum flags , String ceiling_dirs))
		options: #(optCoerceNilToNull optStringOrNull)
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_set_head: refname log_message: log_message [
	
	^ self
		call:
			#(int git_repository_set_head #(self , String refname , String log_message))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_set_head_detached: commitish [
	
	^ self
		call:
			#(int git_repository_set_head_detached #(self, LGitId * commitish))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> repository_workdir: repo [
	
	^ self call: #(String git_repository_workdir #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> resetHardTo: target [

	^ self
		resetHardTo: target
		checkoutOptions: (LGitCheckoutOptions defaults
			checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_force;
			yourself)
]

{ #category : #'libgit-calls' }
LGitRepository >> resetHardTo: target checkoutOptions: options [

	^ self
		resetTo: target
		resetType: LGitResetTypeEnum git_reset_hard
		checkoutOptions: options
]

{ #category : #'libgit-calls' }
LGitRepository >> resetTo: target resetType: aResetType checkoutOptions: checkout_opts [

	^ self
		call: #(LGitReturnCodeEnum git_reset(self, LGitObject target, LGitResetTypeEnum aResetType, LGitCheckoutOptions *checkout_opts))
		options: #()

]

{ #category : #operations }
LGitRepository >> revparse: aString [
	"See git-rev-parse documentation.

	Examples:
		repository revparse: 'HEAD' 	# last commit
		repository revparse: 'HEAD~5'	# 5 commits back from last commit
		repository revparse: 'develop'	# last commit from develop branch
	"

	| object |
	self assert: aString notNil.
	self assert: aString trimBoth notEmpty.
	object := LGitObject of: self.
	self withReturnHandlerDo: [ 
		self
			revparse_single: object
			repo: self
			spec: aString ].
	^ object concreteObject	

]

{ #category : #'libgit-calls' }
LGitRepository >> revparse_single: out repo: repo spec: spec [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_revparse_single #(LGitObject * out , self , String spec))
		options: #()
]

{ #category : #operations }
LGitRepository >> setHead: aLGitReference [
	self withReturnHandlerDo: [ 
		self
			repository_set_head: aLGitReference name
			log_message: 'set head to refs/heads/' , aLGitReference name ]
]

{ #category : #operations }
LGitRepository >> setHeadDetached: commitish [
	self withReturnHandlerDo: [ 
		self repository_set_head_detached: commitish id
	]
]

{ #category : #operations }
LGitRepository >> signatureWithName: name email: email dateTime: dateTime [
	| signaturePointer time |
	signaturePointer := ExternalAddress new.
	time := LGitTime fromDateAndTime: dateTime.
	self
		withReturnHandlerDo: [ self
				new_signature: signaturePointer
				name: name
				email: email
				time: time ].
	^ LGitSignature fromHandle: signaturePointer
]

{ #category : #'libgit-calls' }
LGitRepository >> signature_default: out repo: repo [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_signature_default #(void **out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRepository >> tag_list: tag_names repo: repo [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tag_list #(LGitStringArray * tag_names , LGitRepository repo))
		options: #()
]

{ #category : #accessing }
LGitRepository >> tags [
	| array strings |
	array := LGitStringArray externalNew.
	self withReturnHandlerDo: [
		self
			tag_list: array
			repo: self ].
	strings := array strings.
	array free.
	^ strings
	
]

{ #category : #accessing }
LGitRepository >> workingDirectory [
	^ workingDirectory ifNil: [
			workingDirectory := self withReturnHandlerDo: [
				(self repository_workdir: self) asFileReference ] ]
]

{ #category : #accessing }
LGitRepository >> workingDirectoryName [
	^ self isBare
			ifTrue: [ nil ]
			ifFalse: [ self workingDirectory basename ]
]
