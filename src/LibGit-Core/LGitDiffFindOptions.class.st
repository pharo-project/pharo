Class {
	#name : #LGitDiffFindOptions,
	#superclass : #LGitStructWithDefaults,
	#classVars : [
		'OFFSET_PRIM_BREAK_REWRITE_THRESHOLD',
		'OFFSET_PRIM_COPY_THRESHOLD',
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_METRIC',
		'OFFSET_PRIM_RENAME_FROM_REWRITE_THRESHOLD',
		'OFFSET_PRIM_RENAME_LIMIT',
		'OFFSET_PRIM_RENAME_THRESHOLD',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffFindOptions class >> fieldsDesc [
	^#(
		LGitOptionsVersionsEnum version;

		"Combination of git_diff_find_t values (default GIT_DIFF_FIND_BY_CONFIG).
		 NOTE: if you don't explicitly set this, `diff.renames` could be set
		 to false, resulting in `git_diff_find_similar` doing nothing."
		uint32 flags;

		" Similarity to consider a file renamed (default 50) "
		uint16 rename_threshold;
		" Similarity of modified to be eligible rename source (default 50) "
		uint16 rename_from_rewrite_threshold;
		" Similarity to consider a file a copy (default 50) "
		uint16 copy_threshold;
		" Similarity to split modify into delete/add pair (default 60) "
		uint16 break_rewrite_threshold;

		"Maximum similarity sources to examine for a file (somewhat like
		 git-diff's `-l` option or `diff.renameLimit` config) (default 200)"
		size_t rename_limit;

		" Pluggable similarity metric; pass NULL to use internal metric "
		LGitDiffSimilarityMetric metric;
	)
]

{ #category : #'libgit-calls' }
LGitDiffFindOptions >> diff_find_init_options: opts version: version [
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_diff_find_init_options(LGitDiffFindOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )
]

{ #category : #operations }
LGitDiffFindOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			diff_find_init_options: self
			version: LGitOptionsVersionsEnum git_diff_find_options_version_1 ]
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_break_rewrite_threshold [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_BREAK_REWRITE_THRESHOLD
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_break_rewrite_threshold: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_BREAK_REWRITE_THRESHOLD put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_copy_threshold [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_COPY_THRESHOLD
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_copy_threshold: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_COPY_THRESHOLD put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_metric [
	"This method was automatically generated"
	^ LGitDiffSimilarityMetric fromHandle: (handle referenceStructAt: OFFSET_PRIM_METRIC length: LGitDiffSimilarityMetric byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_metric: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_METRIC put: anObject getHandle length: LGitDiffSimilarityMetric byteSize
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_from_rewrite_threshold [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_RENAME_FROM_REWRITE_THRESHOLD
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_from_rewrite_threshold: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_RENAME_FROM_REWRITE_THRESHOLD put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_limit [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_RENAME_LIMIT
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_limit: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_RENAME_LIMIT put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_threshold [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_RENAME_THRESHOLD
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_rename_threshold: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_RENAME_THRESHOLD put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitDiffFindOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]
