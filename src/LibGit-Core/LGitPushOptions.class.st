"
/**
 * Controls the behavior of a git_push object.
 */

"
Class {
	#name : #LGitPushOptions,
	#superclass : #LGitStructWithDefaults,
	#instVars : [
		'callbacks'
	],
	#classVars : [
		'OFFSET_PRIM_CALLBACKS',
		'OFFSET_PRIM_CUSTOM_HEADERS',
		'OFFSET_PRIM_PB_PARALLELISM',
		'OFFSET_PRIM_PROXY_OPTS',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitPushOptions class >> fieldsDesc [
	^ #(
	LGitOptionsVersionsEnum version;

	"
	 * If the transport being used to push to the remote requires the creation
	 * of a pack file, this controls the number of worker threads used by
	 * the packbuilder when creating that pack file to be sent to the remote.
	 *
	 * If set to 0, the packbuilder will auto-detect the number of threads
	 * to create. The default value is 1.
	 "
	uint pb_parallelism;

	"
	 * Callbacks to use for this push operation
	 "
	LGitRemoteCallbacks callbacks;
		
	"
	* Proxy options to use, by default no proxy is used.
	"
	LGitProxyOptions proxy_opts;

	"
	 * Extra headers for this push operation
	"
	LGitStringArray custom_headers;
)
]

{ #category : #accessing }
LGitPushOptions >> callbacks [
	^ self prim_callbacks
]

{ #category : #accessing }
LGitPushOptions >> callbacks: _callbacks [
	callbacks := _callbacks.
	^ self prim_callbacks: callbacks
]

{ #category : #operations }
LGitPushOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			push_init_options: self
			version: LGitOptionsVersionsEnum git_push_options_version_1 ].
	 self prim_proxy_opts prim_type: LGitProxyTypeEnum git_proxy_auto
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_callbacks [
	"This method was automatically generated"
	^ LGitRemoteCallbacks fromHandle: (handle referenceStructAt: OFFSET_PRIM_CALLBACKS length: LGitRemoteCallbacks byteSize)
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_callbacks: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_CALLBACKS put: anObject getHandle length: LGitRemoteCallbacks byteSize
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_custom_headers [
	"This method was automatically generated"
	^ LGitStringArray fromHandle: (handle referenceStructAt: OFFSET_PRIM_CUSTOM_HEADERS length: LGitStringArray byteSize)
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_custom_headers: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_CUSTOM_HEADERS put: anObject getHandle length: LGitStringArray byteSize
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_pb_parallelism [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_PB_PARALLELISM
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_pb_parallelism: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_PB_PARALLELISM put: anObject
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_proxy_opts [
	"This method was automatically generated"
	^ LGitProxyOptions fromHandle: (handle referenceStructAt: OFFSET_PRIM_PROXY_OPTS length: LGitProxyOptions byteSize)
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_proxy_opts: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_PROXY_OPTS put: anObject getHandle length: LGitProxyOptions byteSize
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitPushOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]

{ #category : #'libgit-calls' }
LGitPushOptions >> push_init_options: options version: version [
	self
		callUnchecked: #(LGitReturnCodeEnum git_push_init_options(LGitPushOptions * self, LGitOptionsVersionsEnum version))
		options: #()
]
