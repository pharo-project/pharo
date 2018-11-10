Class {
	#name : #LGitStringArray,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_COUNT',
		'OFFSET_PRIM_STRINGS'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #accessing }
LGitStringArray class >> fieldsDesc [
	^#(
		LGitExternalStringArray * strings;
		size_t count;
	)
]

{ #category : #'libgit-calls' }
LGitStringArray class >> strarray_free: handle [
	"libgit2 will also free all the strings in this struct"
	^ self
		call: #(void git_strarray_free(void *handle))
		options: #()
]

{ #category : #'instance creation' }
LGitStringArray class >> withAll: stringCollection [
	"Allocate an instance on the external heap and initialize it with
	<stringCollection>. Use autoRelease so the data will be freed when
	the instance is gc'ed."
	^ self externalNew
		count: stringCollection size;
		strings: stringCollection;
		autoRelease;
		yourself
]

{ #category : #accessing }
LGitStringArray >> count [
	^ self prim_count
]

{ #category : #accessing }
LGitStringArray >> count: anInteger [
	self prim_count: anInteger
]

{ #category : #'libgit-fields' }
LGitStringArray >> prim_count [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_COUNT
]

{ #category : #'libgit-fields' }
LGitStringArray >> prim_count: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_PRIM_COUNT put: anObject
]

{ #category : #'libgit-fields' }
LGitStringArray >> prim_strings [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_STRINGS) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitStringArray >> prim_strings: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_STRINGS put: anObject getHandle.
]

{ #category : #accessing }
LGitStringArray >> strings [
	| externalArray |
	self count = 0
		ifTrue: [ ^ #() ].
	externalArray := LGitExternalStringArray 
		onAddress: self prim_strings getHandle 
		size: self count.
	^externalArray asArray
]

{ #category : #accessing }
LGitStringArray >> strings: aCollection [
	| array |
	array := LGitExternalStringArray fromCollection: aCollection.
	self prim_strings: array
]
