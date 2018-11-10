"
/**
 * The callback settings structure
 *
 * Set the callbacks to be called by the remote when informing the user
 * about the progress of the network operations.
 */
"
Class {
	#name : #LGitRemoteCallbacks,
	#superclass : #LGitStructWithDefaults,
	#instVars : [
		'credentialsCallback',
		'certificateCheckCallback',
		'transferProgressCallback',
		'pushTransferProgress'
	],
	#classVars : [
		'OFFSET_PRIM_CERTIFICATE_CHECK',
		'OFFSET_PRIM_COMPLETION',
		'OFFSET_PRIM_CREDENTIALS',
		'OFFSET_PRIM_PACK_PROGRESS',
		'OFFSET_PRIM_PAYLOAD',
		'OFFSET_PRIM_PUSH_NEGOTIATION',
		'OFFSET_PRIM_PUSH_TRANSFER_PROGRESS',
		'OFFSET_PRIM_PUSH_UPDATE_REFERENCE',
		'OFFSET_PRIM_SIDEBAND_PROGRESS',
		'OFFSET_PRIM_TRANSFER_PROGRESS',
		'OFFSET_PRIM_TRANSPORT',
		'OFFSET_PRIM_UPDATE_TIPS',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'instance creation' }
LGitRemoteCallbacks class >> defaults [
	^ super defaults 
		certificateCheck: self newCertificateCheckCallback;
		yourself
]

{ #category : #accessing }
LGitRemoteCallbacks class >> fieldsDesc [
	^#(
		LGitStructVersionsEnum version;
		"
		 * Textual progress from the remote. Text send over the
		 * progress side-band will be passed to this function (this is
		 * the 'counting objects' output.
		 "
		LGitTransportMessageCallback sideband_progress;

		"
		 * Completion is called when different parts of the download
		 * process are done (currently unused).
		 "
		LGitCompletionCallback completion;

		"
		 * This will be called if the remote host requires
		 * authentication in order to connect to it.
		 *
		 * Returning GIT_PASSTHROUGH will make libgit2 behave as
		 * though this field isn't set.
		 "
		LGitCredAcquireCallback credentials;

		"
		 * If cert verification fails, this will be called to let the
		 * user make the final decision of whether to allow the
		 * connection to proceed. Returns 1 to allow the connection, 0
		 * to disallow it or a negative value to indicate an error.
		 "
	   LGitTransportCertificateCheckCallback certificate_check;

		"
		 * During the download of new data, this will be regularly
		 * called with the current count of progress done by the
		 * indexer.
		 "
		LGitTransferProgressCallback transfer_progress;

		"
		 * Each time a reference is updated locally, this function
		 * will be called with information about it.
		 "
		LGitUpdateTipsCallback update_tips;

		"
		 * Function to call with progress information during pack
		 * building. Be aware that this is called inline with pack
		 * building operations, so performance may be affected.
		 "
		LGitPackbuilderProgressCallback pack_progress;

		"
		 * Function to call with progress information during the
		 * upload portion of a push. Be aware that this is called
		 * inline with pack building operations, so performance may be
		 * affected.
		 "
		LGitPushTransferProgressCallback push_transfer_progress;

		"
		 * Called for each updated reference on push. If `status` is
		 * not `NULL`, the update was rejected by the remote server
		 * and `status` contains the reason given.
		 "
		LGitPushUpdateReferenceCallback push_update_reference;

		"
		 * Called once between the negotiation step and the upload. It
		 * provides information about what updates will be performed.
		 "
		LGitPushNegotiationCallback push_negotiation;

		"
		 * Create the transport to use for this operation. Leave NULL
		 * to auto-detect.
		 "
		LGitTransport transport;

		"
		 * This will be passed to each of the callbacks in this struct
		 * as the last parameter.
		 "
		void *payload;
	)
]

{ #category : #private }
LGitRemoteCallbacks class >> newCertificateCheckCallback [
	self flag: #fixMe. "I need to skip the verification of certificate because on macOS, 
	libgit2 has problems to be compiled with Secure framework, and if compiled with 
	openssl instead, openssl for macOS does not checks agains the certificates installed 
	at S.O., so it will fail almost always. This is, of course, a workaround... eventually 
	libgit2 will work :P"
	^ LGitTransportCertificateCheckCallback on: [ :cert :value :host :payload  |
		1 ]
]

{ #category : #private }
LGitRemoteCallbacks class >> newCredentialsCallbackFor: provider [
	^ LGitCredAcquireCallback
		on: [ :output :url :username_from_url :allowed_types :data | 
			"Allowed types are defined in git_credtype_t 
		https://github.com/libgit2/libgit2/blob/HEAD/include/git2/transport.h#L81-111"
			self
				putCredentialsType: allowed_types
				username: username_from_url
				provider: provider
				onto: output ]
]

{ #category : #private }
LGitRemoteCallbacks class >> putCredentialsType: types username: username provider: provider onto: output [  

	(types anyMask: 1 << 1) ifTrue: [ 
		^ provider sshCredentialsClass 
			createFrom: provider 
			username: username
			onto: output ].

	(types anyMask: 1 << 0) ifTrue: [ 
		^ LGitCredentialsPlaintext createFrom: provider onto: output ].

	^ -1
]

{ #category : #'instance creation' }
LGitRemoteCallbacks class >> withProvider: provider [

	^ LGitRemoteCallbacks defaults
		credentials: (self newCredentialsCallbackFor: provider);
		certificateCheck: self newCertificateCheckCallback;
		yourself
]

{ #category : #accessing }
LGitRemoteCallbacks >> certificateCheck: aCallback [
	certificateCheckCallback := aCallback.
	self prim_certificate_check: aCallback
]

{ #category : #accessing }
LGitRemoteCallbacks >> credentials [
	^ self prim_credentials
]

{ #category : #accessing }
LGitRemoteCallbacks >> credentials: aCredentialsCallback [
	credentialsCallback := aCredentialsCallback.
	self prim_credentials: credentialsCallback
]

{ #category : #operations }
LGitRemoteCallbacks >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			remote_init_callbacks: self
			version: LGitStructVersionsEnum git_remote_callbacks_version_1 ]
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_certificate_check [
	"This method was automatically generated"
	^LGitTransportCertificateCheckCallback forAddress: ((handle pointerAt: OFFSET_PRIM_CERTIFICATE_CHECK) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_certificate_check: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CERTIFICATE_CHECK
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_completion [
	"This method was automatically generated"
	^LGitCompletionCallback forAddress: ((handle pointerAt: OFFSET_PRIM_COMPLETION) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_completion: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_COMPLETION
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_credentials [
	"This method was automatically generated"
	^LGitCredAcquireCallback forAddress: ((handle pointerAt: OFFSET_PRIM_CREDENTIALS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_credentials: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CREDENTIALS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_pack_progress [
	"This method was automatically generated"
	^LGitPackbuilderProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PACK_PROGRESS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_pack_progress: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PACK_PROGRESS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_negotiation [
	"This method was automatically generated"
	^LGitPushNegotiationCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PUSH_NEGOTIATION) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_negotiation: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PUSH_NEGOTIATION
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_transfer_progress [
	"This method was automatically generated"
	^LGitPushTransferProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PUSH_TRANSFER_PROGRESS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_transfer_progress: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PUSH_TRANSFER_PROGRESS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_update_reference [
	"This method was automatically generated"
	^LGitPushUpdateReferenceCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PUSH_UPDATE_REFERENCE) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_push_update_reference: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PUSH_UPDATE_REFERENCE
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_sideband_progress [
	"This method was automatically generated"
	^LGitTransportMessageCallback forAddress: ((handle pointerAt: OFFSET_PRIM_SIDEBAND_PROGRESS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_sideband_progress: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_SIDEBAND_PROGRESS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_transfer_progress [
	"This method was automatically generated"
	^LGitTransferProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_TRANSFER_PROGRESS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_transfer_progress: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_TRANSFER_PROGRESS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_transport [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_TRANSPORT) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_transport: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_TRANSPORT put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_update_tips [
	"This method was automatically generated"
	^LGitUpdateTipsCallback forAddress: ((handle pointerAt: OFFSET_PRIM_UPDATE_TIPS) asInteger)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_update_tips: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_UPDATE_TIPS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_version [
	"This method was automatically generated"
	^LGitStructVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitRemoteCallbacks >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]

{ #category : #accessing }
LGitRemoteCallbacks >> pushTransferProgress: aCallback [
	pushTransferProgress := aCallback.
	self prim_push_transfer_progress: aCallback
]

{ #category : #'libgit-calls' }
LGitRemoteCallbacks >> remote_init_callbacks: opts version: version [ 
	^ self
		call: #(LGitReturnCodeEnum git_remote_init_callbacks(LGitRemoteCallbacks * self, LGitStructVersionsEnum version))
		options: #(  )
]

{ #category : #accessing }
LGitRemoteCallbacks >> transferProgress: aCallback [
	transferProgressCallback := aCallback.
	self prim_transfer_progress: aCallback
]
