Class {
	#name : #LGitTreeEntry,
	#superclass : #LGitRepositoryObject,
	#instVars : [
		'parent'
	],
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-finalization' }
LGitTreeEntry class >> freeFunctionSelector [
	^ #tree_entry_free:
]

{ #category : #'libgit-calls' }
LGitTreeEntry class >> tree_entry_free: handle [
	
	^ self
		call: #(void git_tree_entry_free #(void *handle))
		options: #()
]

{ #category : #comparing }
LGitTreeEntry >> = anObject [
	^ self == anObject or: [
		self class == anObject class and: [ 
			self isReady and: [ 
				anObject isReady and: [ 
					self compare: anObject ] ] ] ]
]

{ #category : #private }
LGitTreeEntry >> compare: aTreeEntry [
	^ (self
		tree_entry_cmp: self
		e2: aTreeEntry) isZero
]

{ #category : #traversing }
LGitTreeEntry >> entries [
	^ self object entries
]

{ #category : #accessing }
LGitTreeEntry >> entryByPath: aString [ 
	^ self object entryByPath: aString

]

{ #category : #accessing }
LGitTreeEntry >> entryByPath: aString ifAbsent: aBlock [
	^ self object entryByPath: aString ifAbsent: aBlock

]

{ #category : #accessing }
LGitTreeEntry >> fileMode [
	^ self tree_entry_filemode: self
]

{ #category : #accessing }
LGitTreeEntry >> filename [
	^ self tree_entry_name: self
]

{ #category : #testing }
LGitTreeEntry >> isBlob [
	^ self object isBlob
]

{ #category : #testing }
LGitTreeEntry >> isLeaf [

	^ self object isTree
]

{ #category : #accessing }
LGitTreeEntry >> object [
	| object |
	object := (LGitObject classFor: self type) of: self repository.
	self withReturnHandlerDo: [
		self
			tree_entry_to_object: object
			repo: self repository
			entry: self ].
		
	^ object
]

{ #category : #accessing }
LGitTreeEntry >> objectId [
	^ LGitId fromExternalAddress: (self tree_entry_id: self)
]

{ #category : #printing }
LGitTreeEntry >> objectPrintOn: aStream [ 
	aStream nextPutAll: self filename surroundedBySingleQuotes
]

{ #category : #accessing }
LGitTreeEntry >> parent: aLGitTree [ 
	
	parent := aLGitTree
]

{ #category : #traversing }
LGitTreeEntry >> subTreeEntries [
	^ self object subTreeEntries
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_cmp: e1 e2: e2 [
	
	^ self
		call: #(int git_tree_entry_cmp #(self , LGitTreeEntry e2))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_filemode: entry [
	
	^ self
		call: #(LGitFilemodeEnum git_tree_entry_filemode #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_id: entry [
	
	^ self call: #(void * git_tree_entry_id #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_name: entry [
	
	^ self call: #(String git_tree_entry_name #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_to_object: object_out repo: repo entry: entry [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_entry_to_object #(LGitObject * object_out , LGitRepository repo , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTreeEntry >> tree_entry_type: entry [
	
	^ self
		call: #(LGitObjectTypeEnum git_tree_entry_type #(self))
		options: #()
]

{ #category : #accessing }
LGitTreeEntry >> type [
	^ self tree_entry_type: handle
]
