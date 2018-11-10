Class {
	#name : #LGitCheckoutOptions,
	#superclass : #LGitStructWithDefaults,
	#instVars : [
		'progressCallback'
	],
	#classVars : [
		'OFFSET_PRIM_ANCESTOR_LABEL',
		'OFFSET_PRIM_BASELINE',
		'OFFSET_PRIM_BASELINE_INDEX',
		'OFFSET_PRIM_CHECKOUT_STRATEGY',
		'OFFSET_PRIM_DIR_MODE',
		'OFFSET_PRIM_DISABLE_FILTERS',
		'OFFSET_PRIM_FILE_MODE',
		'OFFSET_PRIM_FILE_OPEN_FLAGS',
		'OFFSET_PRIM_NOTIFY_CB',
		'OFFSET_PRIM_NOTIFY_FLAGS',
		'OFFSET_PRIM_NOTIFY_PAYLOAD',
		'OFFSET_PRIM_OUR_LABEL',
		'OFFSET_PRIM_PATHS',
		'OFFSET_PRIM_PERFDATA_CB',
		'OFFSET_PRIM_PERFDATA_PAYLOAD',
		'OFFSET_PRIM_PROGRESS_CB',
		'OFFSET_PRIM_PROGRESS_PAYLOAD',
		'OFFSET_PRIM_TARGET_DIRECTORY',
		'OFFSET_PRIM_THEIR_LABEL',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #accessing }
LGitCheckoutOptions class >> fieldsDesc [
	^#(
		LGitOptionsVersionsEnum version;

		uint checkout_strategy; "< default will be a dry run "

		int disable_filters;    "< don't apply filters like CRLF conversion "
		uint dir_mode;  "< default is 0755 "
		uint file_mode; "< default is 0644 or 0755 as dictated by blob "
		int file_open_flags;    "< default is O_CREAT | O_TRUNC | O_WRONLY "

		uint notify_flags; "< see `git_checkout_notify_t` above "
		LGitCheckoutNotifyCallback notify_cb;
		void *notify_payload;

		" Optional callback to notify the consumer of checkout progress. "
		LGitCheckoutProgressCallback progress_cb;
		void *progress_payload;

		" When not zeroed out, array of fnmatch patterns specifying which
		 *  paths should be taken into account, otherwise all files.  Use
		 *  GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH to treat as simple list.
		 "
		LGitStringArray paths;

		" The expected content of the working directory; defaults to HEAD.
		 *  If the working directory does not match this baseline information,
		 *  that will produce a checkout conflict.
		 "
		LGitTree *baseline;

		" Like `baseline` above, though expressed as an index.  This
		 *  option overrides `baseline`.
		 "
		LGitIndex *baseline_index; "< expected content of workdir, expressed as an index. "

		String target_directory; "< alternative checkout path to workdir "

		String ancestor_label; "< the name of the common ancestor side of conflicts "
		String our_label; "< the name of the *our* side of conflicts "
		String their_label; "< the name of the *their* side of conflicts "

		" Optional callback to notify the consumer of performance data. "
		LGitCheckoutPerfDataCallback perfdata_cb;
		void *perfdata_payload;
	)
]

{ #category : #accessing }
LGitCheckoutOptions >> checkoutStrategy: aCheckoutStrategyEnum [
	self prim_checkout_strategy: aCheckoutStrategyEnum value
]

{ #category : #'libgit-calls' }
LGitCheckoutOptions >> checkout_init_options: opts version: version [ 
	^ self
		callUnchecked: #(int git_checkout_init_options(LGitCheckoutOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )
]

{ #category : #operations }
LGitCheckoutOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			checkout_init_options: self
			version: LGitOptionsVersionsEnum git_checkout_options_version_1 ]
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_ancestor_label [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_ANCESTOR_LABEL) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_ancestor_label: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_ANCESTOR_LABEL put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_baseline [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_BASELINE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_baseline: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_BASELINE put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_baseline_index [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_BASELINE_INDEX) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_baseline_index: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_BASELINE_INDEX put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_checkout_strategy [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_CHECKOUT_STRATEGY
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_checkout_strategy: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CHECKOUT_STRATEGY put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_dir_mode [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_DIR_MODE
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_dir_mode: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_DIR_MODE put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_disable_filters [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_DISABLE_FILTERS
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_disable_filters: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_DISABLE_FILTERS put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_file_mode [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FILE_MODE
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_file_mode: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FILE_MODE put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_file_open_flags [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_FILE_OPEN_FLAGS
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_file_open_flags: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_FILE_OPEN_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_cb [
	"This method was automatically generated"
	^LGitCheckoutNotifyCallback forAddress: ((handle pointerAt: OFFSET_PRIM_NOTIFY_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_NOTIFY_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_NOTIFY_FLAGS
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_NOTIFY_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_NOTIFY_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_notify_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_NOTIFY_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_our_label [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_OUR_LABEL) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_our_label: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_OUR_LABEL put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_paths [
	"This method was automatically generated"
	^ LGitStringArray fromHandle: (handle referenceStructAt: OFFSET_PRIM_PATHS length: LGitStringArray byteSize)
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_paths: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_PATHS put: anObject getHandle length: LGitStringArray byteSize
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_perfdata_cb [
	"This method was automatically generated"
	^LGitCheckoutPerfDataCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PERFDATA_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_perfdata_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PERFDATA_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_perfdata_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PERFDATA_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_perfdata_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PERFDATA_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_progress_cb [
	"This method was automatically generated"
	^LGitCheckoutProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PROGRESS_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_progress_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PROGRESS_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_progress_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PROGRESS_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_progress_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PROGRESS_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_target_directory [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_TARGET_DIRECTORY) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_target_directory: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_TARGET_DIRECTORY put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_their_label [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_THEIR_LABEL) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_their_label: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_THEIR_LABEL put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitCheckoutOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]

{ #category : #accessing }
LGitCheckoutOptions >> progressCallback: anIceGitCheckoutProgress [ 

	progressCallback := anIceGitCheckoutProgress.
	self prim_progress_cb: anIceGitCheckoutProgress
]
