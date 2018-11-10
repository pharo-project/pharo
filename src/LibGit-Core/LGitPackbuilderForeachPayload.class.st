Class {
	#name : #LGitPackbuilderForeachPayload,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_PROGRESS_CB',
		'OFFSET_PRIM_PROGRESS_PAYLOAD',
		'OFFSET_PRIM_STATS',
		'OFFSET_PRIM_WRITEPACK'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'field definition' }
LGitPackbuilderForeachPayload class >> fieldsDesc [
	^#(
		LGitTransferProgress *stats;
		LGitTransferProgressCallback *progress_cb;
		void *progress_payload;
		LGitOdbWritepack *writepack;
	)
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_progress_cb [
	"This method was automatically generated"
	^LGitTransferProgressCallback forAddress: ((handle pointerAt: OFFSET_PRIM_PROGRESS_CB) asInteger)
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_progress_cb: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_PROGRESS_CB
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_progress_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PROGRESS_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_progress_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PROGRESS_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_stats [
	"This method was automatically generated"
	^LGitTransferProgress fromHandle: (handle pointerAt: OFFSET_PRIM_STATS)
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_stats: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_STATS put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_writepack [
	"This method was automatically generated"
	^LGitOdbWritepack fromHandle: (handle pointerAt: OFFSET_PRIM_WRITEPACK)
]

{ #category : #'libgit-fields' }
LGitPackbuilderForeachPayload >> prim_writepack: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_WRITEPACK put: anObject getHandle.
]

{ #category : #accessing }
LGitPackbuilderForeachPayload >> stats [
	^ self prim_stats
]

{ #category : #accessing }
LGitPackbuilderForeachPayload >> stats: aTransferProgress [
	self prim_stats: aTransferProgress
]

{ #category : #accessing }
LGitPackbuilderForeachPayload >> writepack [
	^ self prim_writepack
]

{ #category : #accessing }
LGitPackbuilderForeachPayload >> writepack: aWritepack [
	self prim_writepack: aWritepack
]
