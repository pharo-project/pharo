Class {
	#name : #LGitPushUpdate,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_DST',
		'OFFSET_PRIM_DST_REFNAME',
		'OFFSET_PRIM_SRC',
		'OFFSET_PRIM_SRC_REFNAME'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitPushUpdate class >> fieldsDesc [
	^#(
		"
		 * The source name of the reference
		 "
		String src_refname;
		"
		 * The name of the reference to update on the server
		 "
		String dst_refname;
		"
		 * The current target of the reference
		 "
		LGitId src
		"
		 * The new target for the reference
		 "
		LGitId dst
	)
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_dst [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_DST length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_dst: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_DST put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_dst_refname [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_DST_REFNAME) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_dst_refname: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_DST_REFNAME put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_src [
	"This method was automatically generated"
	^ LGitId fromHandle: (handle referenceStructAt: OFFSET_PRIM_SRC length: LGitId byteSize)
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_src: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_SRC put: anObject getHandle length: LGitId byteSize
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_src_refname [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_SRC_REFNAME) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitPushUpdate >> prim_src_refname: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_SRC_REFNAME put: anObject getHandle.
]
