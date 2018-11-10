Class {
	#name : #LGitBlameHunk,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_BOUNDARY',
		'OFFSET_PRIM_FINAL_COMMIT_ID',
		'OFFSET_PRIM_FINAL_SIGNATURE',
		'OFFSET_PRIM_FINAL_START_LINE_NUMBER',
		'OFFSET_PRIM_LINES_IN_HUNK',
		'OFFSET_PRIM_ORIG_COMMIT_ID',
		'OFFSET_PRIM_ORIG_PATH',
		'OFFSET_PRIM_ORIG_SIGNATURE',
		'OFFSET_PRIM_ORIG_START_LINE_NUMBER'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #alignment }
LGitBlameHunk class >> byteAlignment [
	^ 1
]

{ #category : #'fields description' }
LGitBlameHunk class >> fieldsDesc [
	<hack: 'orig_start_line_number is a uint16_t according to spec. The offset of orig_signature however is 56, not 54 (while the first fields are *not* aligned). To fix that, I changed the type of orig_start_line_number to uint32_t'>
	^ #(
		size_t lines_in_hunk "is the number of lines in this hunk"
		LGitId final_commit_id "is the OID of the commit where this line was last changed."
		size_t final_start_line_number "is the 1-based line number where this hunk begins, in the final version of the file"
		LGitSignature * final_signature
		LGitId orig_commit_id "is the OID of the commit where this hunk was found. This will usually be the same as final_commit_id, except when GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES has been specified."
		String orig_path "is the path to the file where this hunk originated, as of the commit specified by orig_commit_id."
		size_t orig_start_line_number "is the 1-based line number where this hunk begins in the file named by orig_path in the commit specified by orig_commit_id."
		LGitSignature * orig_signature
		Boolean boundary "is 1 iff the hunk has been tracked to a boundary commit (the root, or the commit specified in git_blame_options.oldest_commit)"
	)
]

{ #category : #accessing }
LGitBlameHunk >> boundary [
	^ self prim_boundary
]

{ #category : #accessing }
LGitBlameHunk >> finalCommitId [
	^ self prim_final_commit_id

]

{ #category : #accessing }
LGitBlameHunk >> finalSignature [	
	^ self prim_final_signature
]

{ #category : #accessing }
LGitBlameHunk >> finalStartLineNumber [
	^ self prim_final_start_line_number
]

{ #category : #accessing }
LGitBlameHunk >> numberOfLines [
	^ self prim_lines_in_hunk
]

{ #category : #accessing }
LGitBlameHunk >> originalCommitId [
	^ self prim_orig_commit_id

]

{ #category : #accessing }
LGitBlameHunk >> originalPath [
	^ self prim_orig_path readString
]

{ #category : #accessing }
LGitBlameHunk >> originalSignature [	
	^ self prim_orig_signature
]

{ #category : #accessing }
LGitBlameHunk >> originalStartLineNumber [
	^ self prim_orig_start_line_number
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_boundary [
	"This method was automatically generated"
	^handle booleanAt: OFFSET_PRIM_BOUNDARY
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_boundary: anObject [
	"This method was automatically generated"
	handle booleanAt: OFFSET_PRIM_BOUNDARY put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_commit_id [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle 
		copyFrom: OFFSET_PRIM_FINAL_COMMIT_ID 
		to: OFFSET_PRIM_FINAL_COMMIT_ID + LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_commit_id: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_FINAL_COMMIT_ID put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_signature [
	"This method was automatically generated"
	^LGitSignature fromHandle: (handle pointerAt: OFFSET_PRIM_FINAL_SIGNATURE)
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_signature: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FINAL_SIGNATURE put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_start_line_number [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_FINAL_START_LINE_NUMBER
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_final_start_line_number: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_FINAL_START_LINE_NUMBER put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_lines_in_hunk [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_LINES_IN_HUNK
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_lines_in_hunk: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_LINES_IN_HUNK put: anObject
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_commit_id [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle 
		copyFrom: OFFSET_PRIM_ORIG_COMMIT_ID 
		to: OFFSET_PRIM_ORIG_COMMIT_ID + LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_commit_id: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_ORIG_COMMIT_ID put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_path [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_ORIG_PATH) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_path: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_ORIG_PATH put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_signature [
	"This method was automatically generated"
	^LGitSignature fromHandle: (handle pointerAt: OFFSET_PRIM_ORIG_SIGNATURE)
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_signature: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_ORIG_SIGNATURE put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_start_line_number [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_ORIG_START_LINE_NUMBER
]

{ #category : #'libgit-fields' }
LGitBlameHunk >> prim_orig_start_line_number: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_ORIG_START_LINE_NUMBER put: anObject
]
