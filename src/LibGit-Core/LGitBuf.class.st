Class {
	#name : #LGitBuf,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_ASIZE',
		'OFFSET_PRIM_PTR',
		'OFFSET_PRIM_SIZE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'field definition' }
LGitBuf class >> fieldsDesc [
	^ #(
		char * ptr;
		size_t asize;
		size_t size;
	)
	
	"self rebuildFieldAccessors"
]

{ #category : #'libgit-calls' }
LGitBuf class >> signature_free: handle [ 
	^ self
		call: #(void git_buf_free(void *handle))
		options: #(  )
]

{ #category : #'libgit-finalization' }
LGitBuf >> free [
	"Free the memory referred to by the git_buf.

	Note that this does not free the git_buf itself, just the memory pointed to by buffer->ptr. This 	will not free the memory if it looks like it was not allocated internally, but it will clear the 	buffer back to the empty state."

	self class signature_free: handle
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_asize [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_ASIZE
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_asize: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_ASIZE put: anObject
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_ptr [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PTR) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_ptr: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PTR put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_size [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_SIZE
]

{ #category : #'libgit-fields' }
LGitBuf >> prim_size: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_SIZE put: anObject
]
