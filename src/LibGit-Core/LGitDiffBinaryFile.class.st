"
/** The contents of one of the files in a binary diff. */
"
Class {
	#name : #LGitDiffBinaryFile,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_DATA',
		'OFFSET_PRIM_DATALEN',
		'OFFSET_PRIM_INFLATEDLEN',
		'OFFSET_PRIM_TYPE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffBinaryFile class >> fieldsDesc [
	^#(
		" The type of binary data for this file. "
		LGitDiffBinaryTypeEnum type;

		" The binary data, deflated. is char* originally, but I do not want to get the \0"
		void *data;

		" The length of the binary data. "
		size_t datalen;

		" The length of the binary data after inflation. "
		size_t inflatedlen;
	)
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_data [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_DATA) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_data: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_DATA put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_datalen [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_DATALEN
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_datalen: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_DATALEN put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_inflatedlen [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_INFLATEDLEN
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_inflatedlen: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_INFLATEDLEN put: anObject
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_type [
	"This method was automatically generated"
	^LGitDiffBinaryTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_TYPE)
]

{ #category : #'libgit-fields' }
LGitDiffBinaryFile >> prim_type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_TYPE put: anObject value
]
