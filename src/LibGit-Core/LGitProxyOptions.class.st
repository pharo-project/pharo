"
/**
 * Options for connecting through a proxy
 *
 * Note that not all types may be supported, depending on the platform
 * and compilation options.
 */
"
Class {
	#name : #LGitProxyOptions,
	#superclass : #LGitStructWithDefaults,
	#classVars : [
		'OFFSET_PRIM_CERTIFICATE_CHECK',
		'OFFSET_PRIM_CREDENTIALS',
		'OFFSET_PRIM_PAYLOAD',
		'OFFSET_PRIM_TYPE',
		'OFFSET_PRIM_URL',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'field definition' }
LGitProxyOptions class >> fieldsDesc [
	^ #(
	LGitOptionsVersionsEnum version;

	"/**
	 * The type of proxy to use, by URL, auto-detect.
	 */"
	LGitProxyTypeEnum type;

	"/**
	 * The URL of the proxy.
	 */"
	String url;

	"/**
	 * This will be called if the remote host requires
	 * authentication in order to connect to it.
	 *
	 * Returning GIT_PASSTHROUGH will make libgit2 behave as
	 * though this field isn't set.
	 */"
	LGitCredAcquireCallback credentials;

	"/**
	 * If cert verification fails, this will be called to let the
	 * user make the final decision of whether to allow the
	 * connection to proceed. Returns 1 to allow the connection, 0
	 * to disallow it or a negative value to indicate an error.
	 */"
   LGitTransportCertificateCheckCallback certificate_check;

	"/**
	 * Payload to be provided to the credentials and certificate
	 * check callbacks.
	 */"
	void *payload;
	)
]

{ #category : #initialization }
LGitProxyOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			proxy_init_options: self getHandle getHandle
			version: LGitOptionsVersionsEnum git_fetch_options_version_1 ]
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_certificate_check [
	"This method was automatically generated"
	^LGitTransportCertificateCheckCallback forAddress: ((handle pointerAt: OFFSET_PRIM_CERTIFICATE_CHECK) asInteger)
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_certificate_check: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CERTIFICATE_CHECK
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_credentials [
	"This method was automatically generated"
	^LGitCredAcquireCallback forAddress: ((handle pointerAt: OFFSET_PRIM_CREDENTIALS) asInteger)
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_credentials: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CREDENTIALS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_type [
	"This method was automatically generated"
	^LGitProxyTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_TYPE)
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_TYPE put: anObject value
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_url [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_URL) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_url: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_URL put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitProxyOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]

{ #category : #'libgit-calls' }
LGitProxyOptions >> proxy_init_options: opts version: version [
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_proxy_init_options(void *handle, LGitOptionsVersionsEnum version))
		options: #(  )
]
