"
I'm a description of a reference advertised by a remote server, given out on `ls` calls.

https://libgit2.github.com/libgit2/#HEAD/type/git_remote_head
"
Class {
	#name : #LGitRemoteHead,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_LOCAL',
		'OFFSET_PRIM_LOID',
		'OFFSET_PRIM_NAME',
		'OFFSET_PRIM_OID',
		'OFFSET_PRIM_SYMREF_TARGET'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'field definition' }
LGitRemoteHead class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^#(
	int local; " available locally "
	LGitId oid;
	LGitId loid;
	String name;
	"
	 * If the server send a symref mapping for this ref, this will
	 * point to the target.
	"
	String symref_target;
	)
]

{ #category : #accessing }
LGitRemoteHead >> name [ 
	^ (self prim_name) readString
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_local [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PRIM_LOCAL
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_local: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_LOCAL put: anObject
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_loid [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_LOID length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_loid: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_LOID put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_name [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_NAME) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_name: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_NAME put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_oid [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_OID length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_oid: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_OID put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_symref_target [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_SYMREF_TARGET) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitRemoteHead >> prim_symref_target: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_SYMREF_TARGET put: anObject getHandle.
]
