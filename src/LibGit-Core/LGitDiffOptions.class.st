Class {
	#name : #LGitDiffOptions,
	#superclass : #LGitStructWithDefaults,
	#classVars : [
		'OFFSET_PRIM_CONTEXT_LINES',
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_ID_ABBREV',
		'OFFSET_PRIM_IGNORE_SUBMODULES',
		'OFFSET_PRIM_INTERHUNK_LINES',
		'OFFSET_PRIM_MAX_SIZE',
		'OFFSET_PRIM_NEW_PREFIX',
		'OFFSET_PRIM_NOTIFY_CB',
		'OFFSET_PRIM_NOTIFY_PAYLOAD',
		'OFFSET_PRIM_OLD_PREFIX',
		'OFFSET_PRIM_PATHSPEC',
		'OFFSET_PRIM_PROGRESS_CB',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffOptions class >> fieldsDesc [
	^#(
		LGitOptionsVersionsEnum version      "version for the struct"
		uint32 flags            "defaults to GIT_DIFF_NORMAL"

		"options controlling which files are in the diff"

		LGitSubmoduleIgnoreTypeEnum ignore_submodules "submodule ignore rule"
		LGitStringArray       pathspec     "defaults to include all paths"
		LGitDiffNotifyCallback notify_cb
		LGitDiffProgressCallback progress_cb
		void *notify_payload

		"options controlling how to diff text is generated"

		uint32    context_lines    "defaults to 3"
		uint32    interhunk_lines  "defaults to 0"
		uint16    id_abbrev       "default 'core.abbrev' or 7 if unset"
		int64   max_size         "defaults to 512MB"
		String old_prefix       "defaults to 'a'"
		String new_prefix       "defaults to 'b'"
	)
]

{ #category : #'libgit-calls' }
LGitDiffOptions >> diff_init_options: opts version: version [
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_diff_init_options(LGitDiffOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )
]

{ #category : #operations }
LGitDiffOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			diff_init_options: self
			version: LGitOptionsVersionsEnum git_diff_options_version_1 ]
]

{ #category : #accessing }
LGitDiffOptions >> pathspec [
	^ self prim_pathspec
]

{ #category : #accessing }
LGitDiffOptions >> pathspec: aPathSpec [
	^ self prim_pathspec: aPathSpec
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_context_lines [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_CONTEXT_LINES
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_context_lines: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CONTEXT_LINES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_id_abbrev [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_ID_ABBREV
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_id_abbrev: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_ID_ABBREV put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_ignore_submodules [
	"This method was automatically generated"
	^LGitSubmoduleIgnoreTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_IGNORE_SUBMODULES)
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_ignore_submodules: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_IGNORE_SUBMODULES put: anObject value
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_interhunk_lines [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_INTERHUNK_LINES
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_interhunk_lines: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_INTERHUNK_LINES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_max_size [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_PRIM_MAX_SIZE
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_max_size: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_PRIM_MAX_SIZE put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_new_prefix [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_NEW_PREFIX) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_new_prefix: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_NEW_PREFIX put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_notify_cb [
	"This method was automatically generated"
	^LGitDiffNotifyCallback forAddress: ((handle pointerAt: OFFSET_PRIM_NOTIFY_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_notify_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_NOTIFY_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_notify_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_NOTIFY_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_notify_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_NOTIFY_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_old_prefix [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_OLD_PREFIX) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_old_prefix: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_OLD_PREFIX put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_pathspec [
	"This method was automatically generated"
	^ LGitStringArray fromHandle: (handle referenceStructAt: OFFSET_PRIM_PATHSPEC length: LGitStringArray byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_pathspec: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_PATHSPEC put: anObject getHandle length: LGitStringArray byteSize
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_progress_cb [
	"This method was automatically generated"
	^LGitDiffProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PROGRESS_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_progress_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PROGRESS_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitDiffOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]
