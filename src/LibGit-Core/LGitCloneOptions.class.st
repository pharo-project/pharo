Class {
	#name : #LGitCloneOptions,
	#superclass : #LGitStructWithDefaults,
	#instVars : [
		'fetchOptions'
	],
	#classVars : [
		'OFFSET_PRIM_BARE',
		'OFFSET_PRIM_CHECKOUT_BRANCH',
		'OFFSET_PRIM_CHECKOUT_OPTS',
		'OFFSET_PRIM_FETCH_OPTS',
		'OFFSET_PRIM_LOCAL',
		'OFFSET_PRIM_REMOTE_CB',
		'OFFSET_PRIM_REMOTE_CB_PAYLOAD',
		'OFFSET_PRIM_REPOSITORY_CB',
		'OFFSET_PRIM_REPOSITORY_CB_PAYLOAD',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #accessing }
LGitCloneOptions class >> fieldsDesc [
	^#(
		LGitOptionsVersionsEnum version;
		
		"
		 * These options are passed to the checkout step. To disable
		 * checkout, set the `checkout_strategy` to
		 * `GIT_CHECKOUT_NONE`.
		 "
		LGitCheckoutOptions checkout_opts;

		"
		 * Options which control the fetch, including callbacks.
		 *
		 * The callbacks are used for reporting fetch progress, and for acquiring
		 * credentials in the event they are needed.
		 "
		LGitFetchOptions fetch_opts;

		"
		 * Set to zero (false) to create a standard repo, or non-zero
		 * for a bare repo
		 "
		Boolean bare;

		"
		 * Whether to use a fetch or copy the object database.
		 "
		LGitCloneLocalTypeEnum local;

		"
		 * The name of the branch to checkout. NULL means use the
		 * remote's default branch.
		 "
		String checkout_branch;

		"
		 * A callback used to create the new repository into which to
		 * clone. If NULL, the 'bare' field will be used to determine
		 * whether to create a bare repository.
		 "
		LGitRepositoryCreateCallback repository_cb;

		"
		 * An opaque payload to pass to the git_repository creation callback.
		 * This parameter is ignored unless repository_cb is non-NULL.
		 "
		void *repository_cb_payload;

		"
		 * A callback used to create the git_remote, prior to its being
		 * used to perform the clone operation. See the documentation for
		 * git_remote_create_cb for details. This parameter may be NULL,
		 * indicating that git_clone should provide default behavior.
		 "
		LGitRemoteCreateCallback remote_cb;

		"
		 * An opaque payload to pass to the git_remote creation callback.
		 * This parameter is ignored unless remote_cb is non-NULL.
		 "
		void *remote_cb_payload;
	)
]

{ #category : #'instance creation' }
LGitCloneOptions class >> withCredentialsCallback: aBlock [
	| credentialsCallback callbacks fetchOptions  |
	credentialsCallback := LGitCredAcquireCallback on: aBlock.

	callbacks := LGitRemoteCallbacks defaults
		credentials: credentialsCallback;
		yourself.

	fetchOptions := LGitFetchOptions defaults
		callbacks: callbacks;
		yourself.

	^ self defaults
		fetchOptions: fetchOptions;
		yourself.
	

]

{ #category : #'instance creation' }
LGitCloneOptions class >> withCredentialsProvider: provider [
	| fetchOptions |
	fetchOptions := LGitFetchOptions defaults
		callbacks: (LGitRemoteCallbacks withProvider: provider);
		yourself.

	^ self defaults
		fetchOptions: fetchOptions
		yourself.
	

]

{ #category : #accessing }
LGitCloneOptions >> checkoutOptions [
	^ self prim_checkout_opts
]

{ #category : #'libgit-calls' }
LGitCloneOptions >> clone_init_options: opts version: version [ 
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_clone_init_options(LGitCloneOptions *self, LGitOptionsVersionsEnum version))
		options: #(  )
]

{ #category : #accessing }
LGitCloneOptions >> fetchOptions [
	^ self prim_fetch_opts
]

{ #category : #accessing }
LGitCloneOptions >> fetchOptions: _fetchOptions [
	fetchOptions := _fetchOptions.
	self prim_fetch_opts: fetchOptions
]

{ #category : #operations }
LGitCloneOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			clone_init_options: self
			version: LGitOptionsVersionsEnum  git_clone_options_version_1 ].
	self prim_fetch_opts prim_proxy_opts prim_type: LGitProxyTypeEnum git_proxy_auto
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_bare [
	"This method was automatically generated"
	^handle booleanAt: OFFSET_PRIM_BARE
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_bare: anObject [
	"This method was automatically generated"
	handle booleanAt: OFFSET_PRIM_BARE put: anObject
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_checkout_branch [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_CHECKOUT_BRANCH) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_checkout_branch: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_CHECKOUT_BRANCH put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_checkout_opts [
	"This method was automatically generated"
	^ LGitCheckoutOptions fromHandle: (handle referenceStructAt: OFFSET_PRIM_CHECKOUT_OPTS length: LGitCheckoutOptions byteSize)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_checkout_opts: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_CHECKOUT_OPTS put: anObject getHandle length: LGitCheckoutOptions byteSize
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_fetch_opts [
	"This method was automatically generated"
	^ LGitFetchOptions fromHandle: (handle referenceStructAt: OFFSET_PRIM_FETCH_OPTS length: LGitFetchOptions byteSize)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_fetch_opts: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_FETCH_OPTS put: anObject getHandle length: LGitFetchOptions byteSize
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_local [
	"This method was automatically generated"
	^LGitCloneLocalTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_LOCAL)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_local: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_LOCAL put: anObject value
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_remote_cb [
	"This method was automatically generated"
	^LGitRemoteCreateCallback forAddress: ((handle pointerAt: OFFSET_PRIM_REMOTE_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_remote_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_REMOTE_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_remote_cb_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_REMOTE_CB_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_remote_cb_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_REMOTE_CB_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_repository_cb [
	"This method was automatically generated"
	^LGitRepositoryCreateCallback forAddress: ((handle pointerAt: OFFSET_PRIM_REPOSITORY_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_repository_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_REPOSITORY_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_repository_cb_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_REPOSITORY_CB_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_repository_cb_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_REPOSITORY_CB_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitCloneOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]

{ #category : #accessing }
LGitCloneOptions >> remoteCallback [
	^ self prim_remote_cb
]

{ #category : #accessing }
LGitCloneOptions >> remoteCallback: aRemoteCallback [
	self prim_remote_cb: aRemoteCallback
]
