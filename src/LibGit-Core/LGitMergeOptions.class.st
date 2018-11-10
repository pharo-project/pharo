Class {
	#name : #LGitMergeOptions,
	#superclass : #LGitStructWithDefaults,
	#classVars : [
		'OFFSET_PRIM_DEFAULT_DRIVER',
		'OFFSET_PRIM_FILE_FAVOR',
		'OFFSET_PRIM_FILE_FLAGS',
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_METRIC',
		'OFFSET_PRIM_RECURSION_LIMIT',
		'OFFSET_PRIM_RENAME_THRESHOLD',
		'OFFSET_PRIM_TARGET_LIMIT',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #accessing }
LGitMergeOptions class >> fieldsDesc [
	"https://libgit2.github.com/libgit2/#HEAD/type/git_merge_options"
	^#(
		LGitOptionsVersionsEnum version;
		uint32 flags; "Should be: git_merge_flag_t"
		uint rename_threshold;
		uint target_limit;
		void* metric; "Should be: git_diff_similarity_metric *"
		uint recursion_limit;
		String default_driver;
		uint32 file_favor; "Should be: git_merge_favor_flag_t"
		uint32 file_flags; "Should be: git_merge_file_flag_t"
	)
]

{ #category : #operations }
LGitMergeOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			merge_init_options: self
			version: LGitOptionsVersionsEnum git_merge_options_version_1 ]
]

{ #category : #'libgit-calls' }
LGitMergeOptions >> merge_init_options: opts version: version [ 
	^ self
		callUnchecked: #(int git_merge_init_options(
			LGitMergeOptions * self, 
			LGitOptionsVersionsEnum version))
		options: #()
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_default_driver [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_DEFAULT_DRIVER) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_default_driver: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_DEFAULT_DRIVER put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_file_favor [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FILE_FAVOR
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_file_favor: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FILE_FAVOR put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_file_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FILE_FLAGS
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_file_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FILE_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_metric [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_METRIC) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_metric: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_METRIC put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_recursion_limit [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_RECURSION_LIMIT
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_recursion_limit: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_RECURSION_LIMIT put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_rename_threshold [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_RENAME_THRESHOLD
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_rename_threshold: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_RENAME_THRESHOLD put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_target_limit [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_TARGET_LIMIT
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_target_limit: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_TARGET_LIMIT put: anObject
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitMergeOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]
