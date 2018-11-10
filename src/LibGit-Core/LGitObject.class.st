Class {
	#name : #LGitObject,
	#superclass : #LGitRepositoryObject,
	#instVars : [
		'id'
	],
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #private }
LGitObject class >> classFor: aType [
	^ self subclasses detect: [ :class | class isType: aType ]
]

{ #category : #'libgit-finalization' }
LGitObject class >> freeFunctionSelector [
	^ #object_free:
]

{ #category : #testing }
LGitObject class >> isType: aType [
	^ self subclassResponsibility
]

{ #category : #'instance creation' }
LGitObject class >> newFromReference: aReference [
	^self of: aReference repository fromId: aReference targetId
]

{ #category : #'libgit-calls' }
LGitObject class >> object_free: handle [ 
	^self call: #(void git_object_free(void *handle)) options: #(  )
]

{ #category : #'instance creation' }
LGitObject class >> of: aRepository fromId: id [
	^ (self of: aRepository)
		initializeWithId: id;
		yourself
]

{ #category : #converting }
LGitObject >> concreteObject [
	^ (self class classFor: self type) of: self repository fromId: self id
]

{ #category : #private }
LGitObject >> get_id_data [
	^ self object_id: self
]

{ #category : #accessing }
LGitObject >> id [
	| data |
	(id notNil or: [ self isNull ]) ifTrue: [ ^ id ].

	data := self get_id_data.
	id := LGitId fromHandle: data getHandle.
	id internalize.
	^ id	

]

{ #category : #initialization }
LGitObject >> initializeWithId: gitOid [
	id := gitOid.
	self lookup
]

{ #category : #testing }
LGitObject >> isBlob [
	^ false
]

{ #category : #testing }
LGitObject >> isCommit [
	^ false
]

{ #category : #testing }
LGitObject >> isTag [
	^ false
]

{ #category : #testing }
LGitObject >> isTree [
	^ false
]

{ #category : #private }
LGitObject >> lookup [
	self withReturnHandlerDo: [ self
		object_lookup: self 
		repo: self repository
		id: self id 
		type: LGitObjectTypeEnum git_obj_any ]
]

{ #category : #accessing }
LGitObject >> object [

	^ self
]

{ #category : #accessing }
LGitObject >> objectPrintOn: aStream [
	self id ifNil: [ ^ self ].
	
	aStream nextPutAll: self id hexString
]

{ #category : #'libgit-calls' }
LGitObject >> object_dup: dest from: source [
	
	^ self
		callUnchecked: #(LGitObjectTypeEnum git_object_dup(LGitObject *dest, LGitObject source))
		options: #()
]

{ #category : #'libgit-calls' }
LGitObject >> object_id: obj [
	
	^ self
		call: #(void * git_object_id #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitObject >> object_lookup: object repo: repo id: oid type: type [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_object_lookup #(LGitObject * object , LGitRepository repo , LGitId * oid , LGitObjectTypeEnum type))
		options: #()
]

{ #category : #'libgit-calls' }
LGitObject >> object_short_id: out [
	
	^ self
		call: #(LGitObjectTypeEnum git_object_short_id(LGitBuf *out, self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitObject >> object_type: obj [
	
	^ self
		call: #(LGitObjectTypeEnum git_object_type #(self))
		options: #()
]

{ #category : #accessing }
LGitObject >> shortId [
	| buf shortId |
	buf := LGitBuf new.
	self object_short_id: buf.
	shortId := buf prim_ptr readString.
	buf free.
	^shortId 
]

{ #category : #accessing }
LGitObject >> type [
	^ self object_type: self
]
