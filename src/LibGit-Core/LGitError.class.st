Class {
	#name : #LGitError,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_KLASS',
		'OFFSET_PRIM_MESSAGE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitError class >> fieldsDesc [
	^#(
	String message;
	LGitErrorTypeEnum klass;
	)
]

{ #category : #'libgit-calls' }
LGitError class >> giterr_clear [
	^ self call: #(void giterr_clear()) options: #(  )
]

{ #category : #'libgit-calls' }
LGitError class >> giterr_last [
	^ self call: #(void *giterr_last()) options: #(  )
]

{ #category : #'libgit-calls' }
LGitError class >> giterr_set_str: string [
	"GIT_EUSER = -7"
	^ self call: #(void giterr_set_str(-7, String string))
]

{ #category : #operations }
LGitError class >> last [
	| externalData |
	externalData := self giterr_last.
	^ self fromHandle: externalData getHandle
]

{ #category : #operations }
LGitError class >> lastMessage [
	^ self last message
]

{ #category : #operations }
LGitError class >> setError: aString [
	self giterr_set_str: aString
]

{ #category : #accessing }
LGitError >> message [
	^ self isNull
		ifFalse: [ self prim_message readString ]
		ifTrue: [ 'no error message set by libgit2' ]
]

{ #category : #'libgit-fields' }
LGitError >> prim_klass [
	"This method was automatically generated"
	^LGitErrorTypeEnum fromInteger: (handle signedLongAt: OFFSET_PRIM_KLASS)
]

{ #category : #'libgit-fields' }
LGitError >> prim_klass: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PRIM_KLASS put: anObject value
]

{ #category : #'libgit-fields' }
LGitError >> prim_message [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_MESSAGE) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitError >> prim_message: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_MESSAGE put: anObject getHandle.
]

{ #category : #accessing }
LGitError >> type [
	^ self isNull
		ifFalse: [ self prim_klass ]
		ifTrue: [ nil ]
]
