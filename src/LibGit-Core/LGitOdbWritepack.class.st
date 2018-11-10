Class {
	#name : #LGitOdbWritepack,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_APPEND',
		'OFFSET_PRIM_BACKEND',
		'OFFSET_PRIM_COMMIT',
		'OFFSET_PRIM_FREE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitOdbWritepack class >> fieldsDesc [
	^#(
	"git_odb_backend *" void *backend; "Using void* because I didn't mapped the structure git_odb_backend"
	LGitWritepackAppendCallout append;
	LGitWritepackCommitCallout commit;
	LGitWritepackFreeCallout free;
	)
]

{ #category : #accessing }
LGitOdbWritepack >> appendWithBuffer: anAddress length: anInteger progress: aTransferProgress [
	| data callout|
	data := self prim_append.
	callout := LGitWritepackAppendCallout fromHandle: data getHandle.

	^ callout
		writepack: self
		buffer: anAddress
		length: anInteger
		progress: aTransferProgress
]

{ #category : #accessing }
LGitOdbWritepack >> commitWithProgress: aTransferProgress [
	self withReturnHandlerDo: [
		self prim_commit
			writepack: self
			progress: aTransferProgress ]
]

{ #category : #accessing }
LGitOdbWritepack >> free [
	self prim_free writepack: self
]

{ #category : #'libgit-calls' }
LGitOdbWritepack >> odb_write_pack: out db: db progress_cb: progress_cb progress_payload: progress_payload [ 
	^ self
		call: #(#LGitReturnCodeEnum git_odb_write_pack(LGitOdbWritepack * self, LGitOdb db, LGitTransferProgressCallback progress_cb, void * progress_payload))
		options: #( optMayGC optCoerceNilToNull )
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_append [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_APPEND) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_append: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_APPEND put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_backend [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_BACKEND) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_backend: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_BACKEND put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_commit [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_COMMIT) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_commit: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_COMMIT put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_free [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_FREE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitOdbWritepack >> prim_free: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FREE put: anObject getHandle.
]
