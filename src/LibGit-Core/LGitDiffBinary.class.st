"
/** Structure describing the binary contents of a diff. */
"
Class {
	#name : #LGitDiffBinary,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_CONTAINS_DATA',
		'OFFSET_PRIM_NEW_FILE',
		'OFFSET_PRIM_OLD_FILE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffBinary class >> fieldsDesc [
	^#(	
		uint contains_data;
		LGitDiffBinaryFile old_file; "< The contents of the old file."
		LGitDiffBinaryFile new_file; "< The contents of the new file."
	)
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_contains_data [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_CONTAINS_DATA
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_contains_data: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CONTAINS_DATA put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_new_file [
	"This method was automatically generated"
	^ LGitDiffBinaryFile fromHandle: (handle referenceStructAt: OFFSET_PRIM_NEW_FILE length: LGitDiffBinaryFile byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_new_file: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_NEW_FILE put: anObject getHandle length: LGitDiffBinaryFile byteSize
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_old_file [
	"This method was automatically generated"
	^ LGitDiffBinaryFile fromHandle: (handle referenceStructAt: OFFSET_PRIM_OLD_FILE length: LGitDiffBinaryFile byteSize)
]

{ #category : #'libgit-fields' }
LGitDiffBinary >> prim_old_file: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_OLD_FILE put: anObject getHandle length: LGitDiffBinaryFile byteSize
]
