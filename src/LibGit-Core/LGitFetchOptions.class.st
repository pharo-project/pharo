Class {
	#name : #LGitFetchOptions,
	#superclass : #LGitStructWithDefaults,
	#instVars : [
		'callbacks'
	],
	#classVars : [
		'OFFSET_PRIM_CALLBACKS',
		'OFFSET_PRIM_CUSTOM_HEADERS',
		'OFFSET_PRIM_DOWNLOAD_TAGS',
		'OFFSET_PRIM_PROXY_OPTS',
		'OFFSET_PRIM_PRUNE',
		'OFFSET_PRIM_UPDATE_FETCHHEAD',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitFetchOptions class >> fieldsDesc [
	^ #(
		LGitOptionsVersionsEnum version;

		"
		 * Callbacks to use for this fetch operation
		 "
		LGitRemoteCallbacks callbacks;

		"
		 * Whether to perform a prune after the fetch
		 "
		LGitFetchPruneTypeEnum prune;

		"
		 * Whether to write the results to FETCH_HEAD. Defaults to
		 * on. Leave this default in order to behave like git.
		 "
		Boolean update_fetchhead;

		"
		 * Determines how to behave regarding tags on the remote, such
		 * as auto-downloading tags for objects we're downloading or
		 * downloading all of them.
		 *
		 * The default is to auto-follow tags.
		 "
		LGitRemoteAutotagOptionTypeEnum download_tags;

		"
	    * Proxy options to use, by default no proxy is used.
	   "
		LGitProxyOptions proxy_opts;

		"
	  	 * Extra headers for this fetch operation
	 	"
		LGitStringArray custom_headers;
	)
]

{ #category : #accessing }
LGitFetchOptions >> callbacks [
	^ self prim_callbacks
]

{ #category : #accessing }
LGitFetchOptions >> callbacks: _callbacks [
	callbacks := _callbacks.
	self prim_callbacks: callbacks
]

{ #category : #'libgit-calls' }
LGitFetchOptions >> fetch_init_options: opts version: version [
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_fetch_init_options(LGitFetchOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )
]

{ #category : #operations }
LGitFetchOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			fetch_init_options: self
			version: LGitOptionsVersionsEnum git_fetch_options_version_1 ].
	self prim_proxy_opts prim_type: LGitProxyTypeEnum git_proxy_auto
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_callbacks [
	"This method was automatically generated"
	^ LGitRemoteCallbacks fromHandle: (handle referenceStructAt: OFFSET_PRIM_CALLBACKS length: LGitRemoteCallbacks byteSize)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_callbacks: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_CALLBACKS put: anObject getHandle length: LGitRemoteCallbacks byteSize
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_custom_headers [
	"This method was automatically generated"
	^ LGitStringArray fromHandle: (handle referenceStructAt: OFFSET_PRIM_CUSTOM_HEADERS length: LGitStringArray byteSize)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_custom_headers: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_CUSTOM_HEADERS put: anObject getHandle length: LGitStringArray byteSize
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_download_tags [
	"This method was automatically generated"
	^LGitRemoteAutotagOptionTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_DOWNLOAD_TAGS)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_download_tags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_DOWNLOAD_TAGS put: anObject value
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_proxy_opts [
	"This method was automatically generated"
	^ LGitProxyOptions fromHandle: (handle referenceStructAt: OFFSET_PRIM_PROXY_OPTS length: LGitProxyOptions byteSize)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_proxy_opts: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_PROXY_OPTS put: anObject getHandle length: LGitProxyOptions byteSize
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_prune [
	"This method was automatically generated"
	^LGitFetchPruneTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_PRUNE)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_prune: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_PRUNE put: anObject value
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_update_fetchhead [
	"This method was automatically generated"
	^handle booleanAt: OFFSET_PRIM_UPDATE_FETCHHEAD
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_update_fetchhead: anObject [
	"This method was automatically generated"
	handle booleanAt: OFFSET_PRIM_UPDATE_FETCHHEAD put: anObject
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitFetchOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]
