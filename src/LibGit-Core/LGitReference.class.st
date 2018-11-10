"
A LGitReference represents the reference to a git object. References can be symbolic as for instance HEAD or a branch etc.

Instance Variables
	handle:			NBExternalObject
	repoHandle:	NBExternalObject

handle
	- the reference to the external object representing the reference

repoHandle
	- the reference to the repository in which the reference is contained

"
Class {
	#name : #LGitReference,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-finalization' }
LGitReference class >> freeFunctionSelector [
	^ #reference_free:
]

{ #category : #'libgit-calls' }
LGitReference class >> reference_free: handle [
	^ self
		call: #(void git_reference_free #(void *handle))
		options: #()
]

{ #category : #comparing }
LGitReference >> = anObject [
	self class == anObject class ifFalse: [ ^ false ].
	
	^ self == anObject or: [
		(self 
			reference_cmp: self 
			ref2: anObject) = LGitReturnCodeEnum git_ok  ]
	
]

{ #category : #accessing }
LGitReference >> annotatedCommit [
	^ LGitAnnotatedCommit fromRef: self
]

{ #category : #accessing }
LGitReference >> basename [
	^ self name copyAfterLast: $/
]

{ #category : #'libgit-calls' }
LGitReference >> branch_is_head: branch [
	
	^ self call: #(int git_branch_is_head #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> branch_set_upstream: upstream [
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_set_upstream #(self , String upstream))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> branch_upstream: out local: local [
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_upstream #(LGitReference * out , self))
		options: #()
]

{ #category : #operations }
LGitReference >> createDirect: nameString target: aGitId force: aBoolean [
	self withReturnHandlerDo: [ 
		self
			reference_create: self
			repo: self repository
			name: nameString
			id: aGitId
			force: aBoolean
			log_message: 'created reference ', nameString ]
]

{ #category : #operations }
LGitReference >> createSymbolic: nameString target: targetString force: aBoolean [
	self withReturnHandlerDo: [ 
		self
			reference_symbolic_create: self
			repo: self repository
			name: nameString
			target: targetString
			force: aBoolean
			log_message: 'created reference ', nameString ]
]

{ #category : #'initialize-release' }
LGitReference >> initializeOn: aHandle in: aRepo [
	handle := aHandle.
	self repository: aRepo.
]

{ #category : #testing }
LGitReference >> isBranch [
	^ (self reference_is_branch: self) = 1
]

{ #category : #testing }
LGitReference >> isDirect [
	^ (self reference_type: self) = LGitReferenceTypeEnum git_ref_oid
]

{ #category : #testing }
LGitReference >> isHead [
	^ (self branch_is_head: self) = 1
]

{ #category : #testing }
LGitReference >> isLocal [
	^ self reference_is_remote not

]

{ #category : #testing }
LGitReference >> isSymbolic [
	^ (self reference_type: self) = LGitReferenceTypeEnum git_ref_symbolic
]

{ #category : #testing }
LGitReference >> isTag [
	^ self reference_is_tag = 1
]

{ #category : #accessing }
LGitReference >> name [
	^ self reference_name: self
]

{ #category : #accessing }
LGitReference >> object [
	self isDirect 
		ifFalse: [ ^ self resolve object ].

	^(LGitObject of: self repository fromId: self targetId) concreteObject
]

{ #category : #printing }
LGitReference >> objectPrintOn: aStream [
	aStream nextPutAll: self name surroundedBySingleQuotes
]

{ #category : #accessing }
LGitReference >> owner [
	^ self call: #(void * git_reference_owner #(self)) options: #()
]

{ #category : #accessing }
LGitReference >> pushUpstream [
	  
	^ self repository config 
		getString: 'remote.pushDefault'
		ifPresent: [ :pushRemoteName |
			self repository 
				lookupBranch: self upstream basename
				inRemote: pushRemoteName ]
		ifAbsent: [ self upstream ]

]

{ #category : #'libgit-calls' }
LGitReference >> reference_cmp: ref1 ref2: ref2 [
	
	^ self
		call: #(LGitReturnCodeEnum git_reference_cmp #(self , LGitReference ref2))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_create: out repo: repo name: name id: id force: force log_message: log_message [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_create #(LGitReference * self , LGitRepository repo , String name , LGitId * id , Boolean force , String log_message))
		options: #(optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitReference >> reference_is_branch: ref [
	
	^ self call: #(int git_reference_is_branch #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_is_remote [
	^ self call: #(Boolean git_reference_is_remote #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_is_tag [	
	^ self call: #(int git_reference_is_tag #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_lookup: out repo: repo name: name [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_lookup #(LGitReference * self , LGitRepository repo , String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_name: ref [
	
	^ self call: #(String git_reference_name #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_resolve: out ref: ref [
	
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_reference_resolve #(LGitReference * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_set_target: out ref: ref id: id log_message: log_message [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_reference_set_target #(LGitReference * out , self , LGitId * id , String log_message))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_symbolic_create: out repo: repo name: name target: target force: force log_message: log_message [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_symbolic_create #(LGitReference * self , LGitRepository repo , String name , String target , Boolean force , String log_message))
		options: #(optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitReference >> reference_symbolic_set_target: out ref: ref target: target log_message: log_message [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_symbolic_set_target #(LGitReference * self , LGitReference ref , String target , String log_message))
		options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_target: ref [
	<todo: 'using LGitId* as return value causes random crashes'>
	
	^ self call: #(void * git_reference_target #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitReference >> reference_type: ref [
	
	^ self
		call: #(LGitReferenceTypeEnum git_reference_type #(self))
		options: #()
]

{ #category : #operations }
LGitReference >> resolve [
	| resolved |
	self isDirect 
		ifTrue: [ ^ self ].
	resolved := self class of: self repository.
	self withReturnHandlerDo: [ self reference_resolve: resolved ref: self ].
	^ resolved
]

{ #category : #operations }
LGitReference >> setTarget: aReference [
	self setTargetId: aReference object id
]

{ #category : #operations }
LGitReference >> setTargetId: aGitId [
	| newRef |
	newRef := LGitReference of: self repository.
	self
		withReturnHandlerDo: [ 
			self
				reference_set_target: newRef
				ref: self
				id: aGitId
				log_message: 'set ' , self name , ' to ' , aGitId hexString ]
]

{ #category : #operations }
LGitReference >> setUpstream: upstreamName [
	self withReturnHandlerDo: [
		self branch_set_upstream: upstreamName
	]

]

{ #category : #accessing }
LGitReference >> shortName [
	^ self isLocal 
		ifTrue: [ self basename ]
		ifFalse: [ self name withoutPrefix: 'refs/remotes/']

]

{ #category : #accessing }
LGitReference >> targetId [
	| data oId |
	data := self reference_target: self.
	data getHandle isNull
		ifTrue: [ ^ nil ].
	oId := LGitId fromExternalAddress: data.
	oId internalize.
	^ oId
]

{ #category : #accessing }
LGitReference >> upstream [
	| upstream | 
	upstream  := self class of: self repository.

	[ self withReturnHandlerDo: [
		self branch_upstream: upstream local: self
	]] 
	on: LGit_GIT_ENOTFOUND do: [ ^ nil ].
	^ upstream

]
