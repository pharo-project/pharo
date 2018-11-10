Class {
	#name : #LGitBlameOptions,
	#superclass : #LGitStructWithDefaults,
	#classVars : [
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_MAX_LINE',
		'OFFSET_PRIM_MIN_LINE',
		'OFFSET_PRIM_MIN_MATCH_CHARACTERS',
		'OFFSET_PRIM_NEWEST_COMMIT',
		'OFFSET_PRIM_OLDEST_COMMIT',
		'OFFSET_PRIM_VERSION'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitBlameOptions class >> fieldsDesc [
	^ #(
		LGitOptionsVersionsEnum version
		uint32 flags "is a combination of the git_blame_flag_t values above."
		uint16 min_match_characters "is the lower bound on the number of alphanumeric characters that must be detected as moving/copying within a file for it to associate those lines with the parent commit. The default value is 20. This value only takes effect if any of the GIT_BLAME_TRACK_COPIES_* flags are specified."
		LGitId newest_commit "is the id of the newest commit to consider. The default is HEAD."
		LGitId oldest_commit "is the id of the oldest commit to consider. The default is the first commit encountered with a NULL parent."
		size_t min_line "is the first line in the file to blame. The default is 1 (line numbers start with 1)."
		size_t max_line "is the last line in the file to blame. The default is the last line of the file."
	)
]

{ #category : #'libgit-calls' }
LGitBlameOptions >> blame_init_options: opts version: version [
	^ self
		callUnchecked: #(int git_blame_init_options(LGitBlameOptions * self, LGitOptionsVersionsEnum version))
		options: #( )
]

{ #category : #operations }
LGitBlameOptions >> initializeWithDefaults [
	self withReturnHandlerDo: [
		self
			blame_init_options: self
			version: LGitOptionsVersionsEnum git_blame_options_version_1 ]
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_max_line [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MAX_LINE
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_max_line: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MAX_LINE put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_min_line [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MIN_LINE
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_min_line: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_MIN_LINE put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_min_match_characters [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_MIN_MATCH_CHARACTERS
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_min_match_characters: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_MIN_MATCH_CHARACTERS put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_newest_commit [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_NEWEST_COMMIT length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_newest_commit: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_NEWEST_COMMIT put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_oldest_commit [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_OLDEST_COMMIT length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_oldest_commit: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_OLDEST_COMMIT put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_version [
	"This method was automatically generated"
	^LGitOptionsVersionsEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_VERSION)
]

{ #category : #'libgit-fields' }
LGitBlameOptions >> prim_version: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_VERSION put: anObject value
]
