"
A LGitSignature represents the information of a specific person for a commit.

Instance Variables
	email:		ByteString
	name:		ByteString
	when:		DateAndTime
"
Class {
	#name : #LGitSignature,
	#superclass : #LGitExternalStructure,
	#classVars : [
		'OFFSET_PRIM_EMAIL',
		'OFFSET_PRIM_NAME',
		'OFFSET_PRIM_WHEN'
	],
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #accessing }
LGitSignature class >> fieldsDesc [
	^#(
		char * name;
		char * email;
		LGitTime when;
	)
]

{ #category : #'instance creation' }
LGitSignature class >> name: nameString email: emailString [
	^ self name: nameString email: emailString when: DateAndTime now
]

{ #category : #'instance creation' }
LGitSignature class >> name: nameString email: emailString when: dateAndTime [
	"Allocate an instance on the external heap and initialize it.
	Use autoRelease so the data will be freed when the instance is gc'ed."
	^ self externalNew
			name: nameString;
			email: emailString;
			dateAndTime: dateAndTime;
			autoRelease;
			yourself
]

{ #category : #'libgit-calls' }
LGitSignature class >> signature_free: handle [ 
	^ self
		call: #(void git_signature_free(void *handle))
		options: #(  )
]

{ #category : #comparing }
LGitSignature >> = anObject [
	^ self == anObject
		or: [ self class == anObject class
			and: [ self name = anObject name
				and: [ self email = anObject email
					and: [ self dateAndTime = anObject dateAndTime ] ] ] ]
]

{ #category : #accessing }
LGitSignature >> dateAndTime [
	^ self prim_when asDateAndTime
]

{ #category : #accessing }
LGitSignature >> dateAndTime: aDateAndTime [
	self prim_when: (LGitTime fromDateAndTime: aDateAndTime)
]

{ #category : #accessing }
LGitSignature >> email [
	^ self prim_email readString
]

{ #category : #accessing }
LGitSignature >> email: aString [
	self prim_email: aString asLGitExternalString
]

{ #category : #comparing }
LGitSignature >> hash [
	^ ((self name hash bitXor: self email hash) bitXor: self email hash) bitXor: self dateAndTime hash
]

{ #category : #accessing }
LGitSignature >> name [
	^ self prim_name readString
]

{ #category : #accessing }
LGitSignature >> name: aString [
	self prim_name: aString asLGitExternalString
]

{ #category : #printing }
LGitSignature >> objectPrintOn: aStream [
	aStream
		nextPutAll: self name;
		nextPutAll: ' <';
		nextPutAll: self email;
		nextPutAll: '> '.
	self dateAndTime printOn: aStream
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_email [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_EMAIL) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_email: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_EMAIL put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_name [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_NAME) type: ExternalType char asPointerType
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_name: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_NAME put: anObject getHandle.
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_when [
	"This method was automatically generated"
	^ LGitTime fromHandle: (handle referenceStructAt: OFFSET_PRIM_WHEN length: LGitTime byteSize)
]

{ #category : #'libgit-fields' }
LGitSignature >> prim_when: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_PRIM_WHEN put: anObject getHandle length: LGitTime byteSize
]
