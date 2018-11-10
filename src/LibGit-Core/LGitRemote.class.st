Class {
	#name : #LGitRemote,
	#superclass : #LGitRepositoryObject,
	#instVars : [
		'remoteName',
		'url',
		'fetchSpec'
	],
	#classVars : [
		'Credentials'
	],
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #accessing }
LGitRemote class >> credentials [
	^ Credentials ifNil: [ Credentials := SmallDictionary new ]
]

{ #category : #convenience }
LGitRemote class >> credentialsPlaintext [
	^ LGitCredentialsPlaintext
		username: self username
		passphrase: self password
]

{ #category : #convenience }
LGitRemote class >> credentialsSsh [
	^ LGitCredentialsSSH
		username: self usernameSsh
		passphrase: self keyPassPhrase
		publicKeyReference: self keyFilePublicPath asFileReference
		privateKeyReference: self keyFilePrivatePath asFileReference
]

{ #category : #'libgit-finalization' }
LGitRemote class >> freeFunctionSelector [
	^ #remote_free:
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyFilePrivatePath [
	^ self credentials
		at: #keyFilePrivatePath
		ifAbsentPut: [ UIManager default request: 'Path to your private key file (/home/foo/.ssh/key)?' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyFilePrivatePath: aString [
	self credentials
		at: #keyFilePrivatePath
		put: aString
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyFilePublicPath [
	^ self credentials
		at: #keyFilePublicPath
		ifAbsentPut: [ UIManager default request: 'Path to your public key file (/home/foo/.ssh/key.pub)?' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyFilePublicPath: aString [
	self credentials
		at: #keyFilePublicPath
		put: aString
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyPassPhrase [
	^ self credentials
		at: #keyPassPhrase
		ifAbsentPut: [ UIManager default request: 'Passphrase of your private key file?' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> keyPassPhrase: aString [
	self credentials
		at: #keyPassPhrase
		put: aString
]

{ #category : #'instance creation' }
LGitRemote class >> of: aRepository named: aString [
	^ self of: aRepository named: aString url: nil
]

{ #category : #'instance creation' }
LGitRemote class >> of: aRepository named: nameString url: urlString [
	^ self
		of: aRepository
		named: nameString
		url: urlString
		fetchSpec: nil
]

{ #category : #'instance creation' }
LGitRemote class >> of: aRepository named: nameString url: urlString fetchSpec: aRefSpec [
	^ (self of: aRepository)
		remoteName: nameString;
		url: urlString;
		fetchSpec: aRefSpec;
		yourself
]

{ #category : #'accessing-credentials' }
LGitRemote class >> password [
	^ self credentials
		at: #password
		ifAbsentPut: [ UIManager default request: 'What''s the password for your account?' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> password: aString [
	self credentials
		at: #password
		put: aString
]

{ #category : #'libgit-calls' }
LGitRemote class >> remote_free: handle [
	
	^ self call: #(void git_remote_free #(void *handle)) options: #()
]

{ #category : #'accessing-credentials' }
LGitRemote class >> resetCredentials [
	Credentials := nil
]

{ #category : #'accessing-credentials' }
LGitRemote class >> username [
	^ self credentials
		at: #username
		ifAbsentPut: [ UIManager default request: 'What''s your account name?' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> username: aString [
	self credentials
		at: #username
		put: aString
]

{ #category : #'accessing-credentials' }
LGitRemote class >> usernameSsh [
	^ self credentials
		at: #usernameSsh
		ifAbsentPut: [ 
			UIManager default 
				request: 'What''s your username for SSH (probably "git")?'
				initialAnswer: 'git' ]
]

{ #category : #'accessing-credentials' }
LGitRemote class >> usernameSsh: aString [
	self credentials
		at: #usernameSsh
		put: aString
]

{ #category : #operations }
LGitRemote >> addFetch [
	self withReturnHandlerDo: [ 
		self
			remote_add_fetch: self repository
			remote: self remoteName
			refspec: self fetchSpec ]
]

{ #category : #operations }
LGitRemote >> addPush [
	self withReturnHandlerDo: [ 
		self
			remote_add_push: self repository
			remote: self remoteName
			refspec: self fetchSpec ]
]

{ #category : #accessing }
LGitRemote >> connectFetch [
	self withReturnHandlerDo: [ 
		self 
			remote_connect: self 
			direction: LGitDirectionEnum git_direction_fetch
			callbacks: LGitRemoteCallbacks defaults ]
]

{ #category : #accessing }
LGitRemote >> connectFetchWithCredentials: aCredentialsProvider [
	self withReturnHandlerDo: [ 
		self 
			remote_connect: self 
			direction: LGitDirectionEnum git_direction_fetch
			callbacks: (LGitRemoteCallbacks withProvider: aCredentialsProvider) ]
]

{ #category : #accessing }
LGitRemote >> connectPushWithCredentials: aCredentialsProvider [
	self withReturnHandlerDo: [ 
		self 
			remote_connect: self 
			direction: LGitDirectionEnum git_direction_push
			callbacks: (LGitRemoteCallbacks withProvider: aCredentialsProvider) ]
]

{ #category : #operations }
LGitRemote >> create [
	<todo: 'use exceptions instead of assertions'>
	self assert: url notNil.
	self assert: self repository isReady.
	
	[ self hasFetchSpec
		ifFalse: [ self createWithoutSpec ]
		ifTrue: [ self createWithFetchSpec ] ]
			on: LGit_GIT_EEXISTS
			do: [ "all good then" ]
]

{ #category : #private }
LGitRemote >> createWithFetchSpec [
	self withReturnHandlerDo: [ 
		self
			remote_create_with_fetchspec: self
			repo: self repository
			name: self remoteName
			url: self url
			fetch: self fetchSpec ]
]

{ #category : #private }
LGitRemote >> createWithoutSpec [
	self withReturnHandlerDo: [ 
		self
			remote_create: self
			repo: self repository
			name: self remoteName
			url: self url ]
]

{ #category : #operations }
LGitRemote >> delete [
	self withReturnHandlerDo: [
		self
			remote_delete: self repository
			name: self remoteName ]
]

{ #category : #operations }
LGitRemote >> fetch [
	self isReady ifFalse: [ self create ].

	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: self ffiNull handle
			opts: (LGitFetchOptions defaults
				callbacks: LGitRemoteCallbacks defaults;
				yourself)
			reflog_message: nil ]
]

{ #category : #accessing }
LGitRemote >> fetchSpec [
	^ fetchSpec
]

{ #category : #accessing }
LGitRemote >> fetchSpec: aRefSpec [
	fetchSpec := aRefSpec
]

{ #category : #operations }
LGitRemote >> fetchWithCredentials: aCredentialsProvider [
	
	^ self
		fetchWithCredentials: aCredentialsProvider
		refSpec: nil
]

{ #category : #operations }
LGitRemote >> fetchWithCredentials: aCredentialsProvider andProgressCallback: progressCallback [
	
	^ self
		fetchWithCredentials: aCredentialsProvider
		andProgressCallback: progressCallback
		refSpec: nil
]

{ #category : #operations }
LGitRemote >> fetchWithCredentials: aCredentialsProvider andProgressCallback: progressCallback refSpec: aRefSpec [
	| refSpecs callbacks |
	self isReady ifFalse: [ self create ].
	refSpecs := aRefSpec ifNotNil: [LGitStringArray withAll: { aRefSpec asString }].
	self withReturnHandlerDo: [ 
		
		callbacks := LGitRemoteCallbacks withProvider: aCredentialsProvider.
		progressCallback ifNotNil: [callbacks transferProgress: progressCallback].
		
		self
			remote_fetch: self
			refspecs: refSpecs
			opts: (LGitFetchOptions defaults
				callbacks:callbacks;
				yourself)
			reflog_message: nil ]
]

{ #category : #operations }
LGitRemote >> fetchWithCredentials: aCredentialsProvider refSpec: aRefSpec [

	^ self fetchWithCredentials: aCredentialsProvider andProgressCallback: nil refSpec: aRefSpec
]

{ #category : #operations }
LGitRemote >> fetchWithMessage: aString [
	self create.
		
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: LGitFetchOptions defaults
			reflog_message: aString ]
]

{ #category : #operations }
LGitRemote >> fetchWithMessage: aString credentials: aCredentialsProvider [ 
	self create.
		
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: (LGitFetchOptions defaults
				callbacks: (LGitRemoteCallbacks withProvider: aCredentialsProvider);
				yourself)
			reflog_message: aString ]
]

{ #category : #testing }
LGitRemote >> hasFetchSpec [
	^ self fetchSpec notNil 
]

{ #category : #operations }
LGitRemote >> lookup [
	self withReturnHandlerDo: [ 
		self
			remote_lookup: self
			repo: self repository
			name: self remoteName.
		self url: self remote_url ].
	self flag: #todo. "shouldn't be autoreleased?"
]

{ #category : #accessing }
LGitRemote >> ls [
	"I list all HEADS in connected remotes (you will need to #connectFetch or 
	#connectPushWithCredentials: first)"
	| out sizeBuf heads size |
	
	sizeBuf := ByteArray new: FFISizeT externalTypeSize.
	out := ExternalAddress new.
	
	self withReturnHandlerDo: [ 
		self remote_ls: out size: sizeBuf ].
	
	size := sizeBuf signedLongAt: 1.
	heads := Array new: size. 
	1 to: size do: [ :index | 
		heads at: index put: (LGitRemoteHead fromHandle: (out pointerAt: 1)).
		out := out + FFIExternalType pointerSize ].
	
	^ heads
]

{ #category : #printing }
LGitRemote >> objectPrintOn: aStream [
	aStream nextPutAll: self remoteName
]

{ #category : #operations }
LGitRemote >> pullWithFetchOptions: fetchOptions [
	<todo: 'check for conflicts'>
	self repository isReady ifFalse: [  ^ self ].

	self lookup.
	
	self withReturnHandlerDo: [ 
		self
			remote_fetch: self
			refspecs: nil
			opts: fetchOptions
			reflog_message: 'automatic pull from libgit2' ].
	
	"assume that there are no conflicts and fast-forwad to FETCH_HEAD"
	self repository fastForward
]

{ #category : #operations }
LGitRemote >> pushWithRefSpec: aRefSpec pushOptions: pushOptions [
	| refspecs return |
	refspecs := LGitStringArray withAll: { aRefSpec asString }.
	return := self withReturnHandlerDo: [
		self
			remote_push: self
			refspecs: refspecs
			opts: pushOptions ].
		
	^ return
]

{ #category : #accessing }
LGitRemote >> refSpecs [ 
	| specs |
	specs := LGitStringArray externalNew autoRelease.
	self withReturnHandlerDo: [ 
		self remote_get_fetch_refspecs: specs ].
	^ specs strings
]

{ #category : #accessing }
LGitRemote >> remoteName [
	^ remoteName
]

{ #category : #accessing }
LGitRemote >> remoteName: aString [
	remoteName := aString
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_add_fetch: repo remote: remote refspec: refspec [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_add_fetch #(LGitRepository repo , String remote String refspec))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_add_push: repo remote: remote refspec: refspec [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_add_push #(LGitRepository repo , String remote , String refspec))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_connect: remote direction: direction callbacks: remoteCallbacks [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_connect #(self , LGitDirectionEnum direction , LGitRemoteCallbacks * remoteCallbacks))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_create: out repo: repo name: name url: urlString [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_remote_create #(LGitRemote * self , LGitRepository repo , String name , String urlString))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_create_with_fetchspec: out repo: repo name: name url: urlString fetch: fetch [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_remote_create_with_fetchspec #(LGitRemote * self , LGitRepository repo , String name , String urlString , String fetch))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_delete: repo name: name [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_delete #(LGitRepository repo , String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_fetch: remote refspecs: refspecs opts: opts reflog_message: reflog_message [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_fetch #(self , LGitStringArray * refspecs , LGitFetchOptions * opts , String reflog_message))
		options: #(optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_get_fetch_refspecs: array [
	^ self call: #(int git_remote_get_fetch_refspecs(LGitStringArray *array, self))
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_lookup: out repo: repo name: name [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_remote_lookup #(LGitRemote * self , LGitRepository repo , String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_ls: out size: size [
	"
	Since I will take a lost of addresses, I'm interested on 2nd level of 'out', that's why 
	I transform `const git_remote_head ***out` into `void **out`
	This is the original function: 
		int git_remote_ls(const git_remote_head ***out, size_t *size, git_remote *remote)"
	^ self call: #( LGitReturnCodeEnum git_remote_ls ( void **out, size_t *size, self ))
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_push: remote refspecs: refspecs opts: opts [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_push #(self , LGitStringArray * refspecs , LGitPushOptions * opts))
		options: #(optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_set_url: repo remote: remote url: urlString [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_set_url #(LGitRepository repo , String remote , String urlString))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_update_tips: remote callbacks: remoteCallbacks update_fetchhead: update_fetchhead download_tags: download_tags reflog_message: reflog_message [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_remote_update_tips #(self , LGitRemoteCallbacks * remoteCallbacks , Boolean update_fetchhead , LGitRemoteAutotagOptionTypeEnum download_tags , String reflog_message))
		options: #(optStringOrNull)
]

{ #category : #'libgit-calls' }
LGitRemote >> remote_url [
	^ self
		call: #(String git_remote_url #(self))
		options: #()
]

{ #category : #operations }
LGitRemote >> setUrl: aString [
	self withReturnHandlerDo: [ 
		self
			remote_set_url: self repository
			remote: self remoteName
			url: aString]
]

{ #category : #accessing }
LGitRemote >> url [
	^ url
]

{ #category : #accessing }
LGitRemote >> url: aString [
	url := aString
]
