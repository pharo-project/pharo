Class {
	#name : #LGitTransferProgress,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_INDEXED_DELTAS',
		'OFFSET_PRIM_INDEXED_OBJECTS',
		'OFFSET_PRIM_LOCAL_OBJECTS',
		'OFFSET_PRIM_RECEIVED_BYTES',
		'OFFSET_PRIM_RECEIVED_OBJECTS',
		'OFFSET_PRIM_TOTAL_DELTAS',
		'OFFSET_PRIM_TOTAL_OBJECTS'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #accessing }
LGitTransferProgress class >> fieldsDesc [
	^#(
	uint total_objects;
	uint indexed_objects;
	uint received_objects;
	uint local_objects;
	uint total_deltas;
	uint indexed_deltas;
	size_t received_bytes;
	)
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_indexed_deltas [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_INDEXED_DELTAS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_indexed_deltas: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_INDEXED_DELTAS put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_indexed_objects [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_INDEXED_OBJECTS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_indexed_objects: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_INDEXED_OBJECTS put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_local_objects [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_LOCAL_OBJECTS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_local_objects: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_LOCAL_OBJECTS put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_received_bytes [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_RECEIVED_BYTES
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_received_bytes: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_RECEIVED_BYTES put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_received_objects [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_RECEIVED_OBJECTS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_received_objects: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_RECEIVED_OBJECTS put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_total_deltas [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_TOTAL_DELTAS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_total_deltas: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_TOTAL_DELTAS put: anObject
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_total_objects [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_PRIM_TOTAL_OBJECTS
]

{ #category : #'libgit-fields' }
LGitTransferProgress >> prim_total_objects: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_TOTAL_OBJECTS put: anObject
]
