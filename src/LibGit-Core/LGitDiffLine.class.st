Class {
	#name : #LGitDiffLine,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_CONTENT',
		'OFFSET_PRIM_CONTENT_LEN',
		'OFFSET_PRIM_CONTENT_OFFSET',
		'OFFSET_PRIM_NEW_LINENO',
		'OFFSET_PRIM_NUM_LINES',
		'OFFSET_PRIM_OLD_LINENO',
		'OFFSET_PRIM_ORIGIN'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffLine class >> fieldsDesc [
	^#(
	LGitDiffLineTypeEnum	  origin       "A git_diff_line_t value"
	int    old_lineno   "Line number in old file or -1 for added line"
	int    new_lineno   "Line number in new file or -1 for deleted line"
	int    num_lines    "Number of newline characters in content"
	size_t content_len  "Number of bytes of data"
	int64 content_offset "Offset in the original file to the content"
	String content "Pointer to diff text, not NUL-byte terminated"
	) 
]

{ #category : #accessing }
LGitDiffLine >> content [
	^ (self prim_content getHandle copyFrom: 1 to: self prim_content_len) asString
]

{ #category : #accessing }
LGitDiffLine >> newLineNumber [
	^ self prim_new_lineno
]

{ #category : #accessing }
LGitDiffLine >> oldLineNumber [
	^ self prim_old_lineno
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_CONTENT) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_CONTENT put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content_len [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_CONTENT_LEN
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content_len: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_CONTENT_LEN put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content_offset [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_PRIM_CONTENT_OFFSET
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_content_offset: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_PRIM_CONTENT_OFFSET put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_new_lineno [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_NEW_LINENO
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_new_lineno: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_NEW_LINENO put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_num_lines [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_NUM_LINES
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_num_lines: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_NUM_LINES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_old_lineno [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_OLD_LINENO
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_old_lineno: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_OLD_LINENO put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_origin [
	"This method was automatically generated"
	^LGitDiffLineTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_ORIGIN)
]

{ #category : #'libgit-fields' }
LGitDiffLine >> prim_origin: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_ORIGIN put: anObject value
]

{ #category : #accessing }
LGitDiffLine >> size [
	^ self prim_content_len
]
