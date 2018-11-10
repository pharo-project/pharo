Class {
	#name : #LGitDiffHunk,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'Char128',
		'OFFSET_PRIM_HEADER',
		'OFFSET_PRIM_HEADER_LEN',
		'OFFSET_PRIM_NEW_LINES',
		'OFFSET_PRIM_NEW_START',
		'OFFSET_PRIM_OLD_LINES',
		'OFFSET_PRIM_OLD_START'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffHunk class >> fieldsDesc [
	^#(
	int old_start "Starting line number in old_file"
	int old_lines "Number of lines in old_file"
	int new_start "Starting line number in new_file"
	int new_lines "Number of lines in new_file"
	size_t header_len "Number of bytes in header text"
	Char128 header "Header text, NUL-byte terminated" 
		)
]

{ #category : #initialization }
LGitDiffHunk class >> initialize [
	Char128 := FFITypeArray ofType: #char size: 128
]

{ #category : #'libgit-calls' }
LGitDiffHunk >> diff_patch_get_hunk: out lines_in_hunk: lines_in_hunk patch: patch hunk_idx: hunk_idx [
	^ self
		call: #(LGitReturnCodeEnum git_patch_get_hunk(LGitDiffHunk * self, LGitWriteBackValueSizeT lines_in_hunk, LGitPatch patch, size_t hunk_idx))
		options: #(  )
]

{ #category : #accessing }
LGitDiffHunk >> newNumberOfLines [
	^ self prim_new_lines
]

{ #category : #accessing }
LGitDiffHunk >> newStart [
	^ self prim_new_start
]

{ #category : #accessing }
LGitDiffHunk >> oldNumberOfLines [
	^ self prim_old_lines
]

{ #category : #accessing }
LGitDiffHunk >> oldStart [
	^ self prim_old_start
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_header [
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFICharacterType size: 128) fromHandle: (handle copyFrom: OFFSET_PRIM_HEADER to: OFFSET_PRIM_HEADER + 127)
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_header: anObject [
	"This method was automatically generated"
	handle replaceFrom: OFFSET_PRIM_HEADER to: OFFSET_PRIM_HEADER + 127 with: anObject getHandle startingAt: 1
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_header_len [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_HEADER_LEN
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_header_len: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_HEADER_LEN put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_new_lines [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_NEW_LINES
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_new_lines: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_NEW_LINES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_new_start [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_NEW_START
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_new_start: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_NEW_START put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_old_lines [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_OLD_LINES
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_old_lines: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_OLD_LINES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_old_start [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_OLD_START
]

{ #category : #'libgit-fields' }
LGitDiffHunk >> prim_old_start: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_OLD_START put: anObject
]
