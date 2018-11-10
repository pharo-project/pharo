Class {
	#name : #LGitCredentials,
	#superclass : #LGitExternalStructure,
	#traits : 'TLGitCalloutTrait',
	#classTraits : 'TLGitCalloutTrait classTrait',
	#instVars : [
		'username',
		'passphrase'
	],
	#classVars : [
		'OFFSET_PRIM_CREDTYPE',
		'OFFSET_PRIM_FREE'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'fields description' }
LGitCredentials class >> fieldsDesc [
	^#(
	LGitCredentialsTypeEnum credtype;
	LGitCredentialsFreeCallout free;
	)
]

{ #category : #operations }
LGitCredentials >> createWith: aStructure [
	self subclassResponsibility
]

{ #category : #accessing }
LGitCredentials >> passphrase [
	^ passphrase
]

{ #category : #accessing }
LGitCredentials >> passphrase: aString [
	passphrase := aString
]

{ #category : #'libgit-fields' }
LGitCredentials >> prim_credtype [
	"This method was automatically generated"
	^LGitCredentialsTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_CREDTYPE)
]

{ #category : #'libgit-fields' }
LGitCredentials >> prim_credtype: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CREDTYPE put: anObject value
]

{ #category : #'libgit-fields' }
LGitCredentials >> prim_free [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_FREE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCredentials >> prim_free: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FREE put: anObject getHandle.
]

{ #category : #accessing }
LGitCredentials >> username [
	^ username
]

{ #category : #accessing }
LGitCredentials >> username: aString [
	username := aString
]
