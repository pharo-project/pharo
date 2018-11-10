Class {
	#name : #LGitDiffSimilarityMetric,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_BUFFER_SIGNATURE',
		'OFFSET_PRIM_FILE_SIGNATURE',
		'OFFSET_PRIM_FREE_SIGNATURE',
		'OFFSET_PRIM_PAYLOAD',
		'OFFSET_PRIM_SIMILARITY'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitDiffSimilarityMetric class >> fieldsDesc [
"
int (*file_signature)(
		void **out, const git_diff_file *file,
		const char *fullpath, void *payload);
	int (*buffer_signature)(
		void **out, const git_diff_file *file,
		const char *buf, size_t buflen, void *payload);
	void (*free_signature)(void *sig, void *payload);
	int (*similarity)(int *score, void *siga, void *sigb, void *payload);
	void *payload;
"
	^#(
	LGitFileSignatureCallback file_signature;
	LGitBufferSignatureCallback buffer_signature;
	LGitFreeSignatureCallback free_signature;
	LGitSimilarityCallback similarity;
	void *payload;
	)
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_buffer_signature [
	"This method was automatically generated"
	^LGitBufferSignatureCallback forAddress: ((handle pointerAt: OFFSET_PRIM_BUFFER_SIGNATURE) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_buffer_signature: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_BUFFER_SIGNATURE
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_file_signature [
	"This method was automatically generated"
	^LGitFileSignatureCallback forAddress: ((handle pointerAt: OFFSET_PRIM_FILE_SIGNATURE) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_file_signature: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_FILE_SIGNATURE
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_free_signature [
	"This method was automatically generated"
	^LGitFreeSignatureCallback forAddress: ((handle pointerAt: OFFSET_PRIM_FREE_SIGNATURE) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_free_signature: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_FREE_SIGNATURE
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_payload [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_PAYLOAD) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_payload: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_PAYLOAD put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_similarity [
	"This method was automatically generated"
	^LGitSimilarityCallback forAddress: ((handle pointerAt: OFFSET_PRIM_SIMILARITY) asInteger)
]

{ #category : #'libgit-fields' }
LGitDiffSimilarityMetric >> prim_similarity: anObject [
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_SIMILARITY
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])
]
