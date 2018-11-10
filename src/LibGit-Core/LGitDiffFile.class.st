Class {
	#name : #LGitDiffFile,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_ID',
		'OFFSET_PRIM_ID_ABBREV',
		'OFFSET_PRIM_MODE',
		'OFFSET_PRIM_PATH',
		'OFFSET_PRIM_SIZE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffFile class >> fieldsDesc [
	^#(
	LGitId id
	String path
	int64 size
	uint32 flags
	uint16 mode
	uint16 id_abbrev
	)
]

{ #category : #accessing }
LGitDiffFile >> id [
	^ self prim_id
]

{ #category : #accessing }
LGitDiffFile >> path [
	^ self prim_path readString
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_id [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_ID length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_id: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_ID put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_id_abbrev [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_ID_ABBREV
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_id_abbrev: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_ID_ABBREV put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_mode [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_MODE
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_mode: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_MODE put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_path [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PATH) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_path: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PATH put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_size [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_PRIM_SIZE
]

{ #category : #'libgit-fields' }
LGitDiffFile >> prim_size: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_PRIM_SIZE put: anObject
]

{ #category : #accessing }
LGitDiffFile >> size [
	^ self prim_size
]
