"
I represent the difference between two entities (e.g. two files). If one of the entities is new or deleted the ""nfiles"" field will hold be set to 1, otherwise it will be set to 2.
"
Class {
	#name : #LGitDiffDelta,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_FLAGS',
		'OFFSET_PRIM_NEW_FILE',
		'OFFSET_PRIM_NFILES',
		'OFFSET_PRIM_OLD_FILE',
		'OFFSET_PRIM_SIMILARITY',
		'OFFSET_PRIM_STATUS'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffDelta class >> fieldsDesc [
	"Since it seems to be a problem with the alignment of this structure in particular
	 and windows, we need to create a special definition (to ensure everything will go 
	 to the correct place). This is a bit ugly, but is the easiest way I found."
	OSPlatform current isWindows 
		ifTrue: [ ^ self fieldsDescForWindows ]. 
	
	^#(	
	LGitDeltaTypeEnum status
	uint32 flags
	"for RENAMED and COPIED, value 0-100"
	uint16 similarity
	"number of files in this delta"
	uint16 nfiles
	LGitDiffFile old_file
	LGitDiffFile new_file
	)
]

{ #category : #'fields description' }
LGitDiffDelta class >> fieldsDescForWindows [
	^#(	
	LGitDeltaTypeEnum status
	uint32 flags
	"for RENAMED and COPIED, value 0-100"
	uint32 similarity
	"number of files in this delta"
	uint32 nfiles
	LGitDiffFile old_file
	LGitDiffFile new_file
	)
]

{ #category : #accessing }
LGitDiffDelta >> newFile [
	^ self prim_new_file
]

{ #category : #accessing }
LGitDiffDelta >> numberOfFiles [
	^ self prim_nfiles
]

{ #category : #accessing }
LGitDiffDelta >> oldFile [
	^ self prim_old_file
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_FLAGS
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_FLAGS put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_new_file [
	"This method was automatically generated"
	^ LGitDiffFile fromHandle: (handle referenceStructAt: OFFSET_PRIM_NEW_FILE length: LGitDiffFile byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_new_file: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_NEW_FILE put: anObject getHandle length: LGitDiffFile byteSize
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_nfiles [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_NFILES
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_nfiles: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_NFILES put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_old_file [
	"This method was automatically generated"
	^ LGitDiffFile fromHandle: (handle referenceStructAt: OFFSET_PRIM_OLD_FILE length: LGitDiffFile byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_old_file: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_OLD_FILE put: anObject getHandle length: LGitDiffFile byteSize
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_similarity [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PRIM_SIMILARITY
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_similarity: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PRIM_SIMILARITY put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_status [
	"This method was automatically generated"
	^LGitDeltaTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_STATUS)
]

{ #category : #'libgit-fields' }
LGitDiffDelta >> prim_status: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_STATUS put: anObject value
]

{ #category : #accessing }
LGitDiffDelta >> status [
	^ self prim_status
]
