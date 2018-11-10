Class {
	#name : #LGitTreeBuilder,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'libgit-finalization' }
LGitTreeBuilder class >> freeFunctionSelector [
	^ #treebuilder_free:
]

{ #category : #'instance creation' }
LGitTreeBuilder class >> fromTree: aTree [
	self validate: aTree shouldBe: LGitTree.
	^ self of: aTree repository fromTree: aTree
]

{ #category : #'instance creation' }
LGitTreeBuilder class >> of: repository fromTree: aTree [
	^ (self of: repository)
		initializeWithTree: aTree;
		yourself
]

{ #category : #'libgit-calls' }
LGitTreeBuilder class >> treebuilder_free: handle [
	
	^ self
		call: #(void git_treebuilder_free #(void *handle))
		options: #()
]

{ #category : #operations }
LGitTreeBuilder >> addEntry: aLGitObject filename: aString [
	self 
		addEntryWithId: aLGitObject id 
		filename: aString 
		filemode: aLGitObject class defaultFilemode
]

{ #category : #operations }
LGitTreeBuilder >> addEntryWithId: oidStruct filename: aString filemode: anInteger [
	self withReturnHandlerDo: [ 
			self
				treebuilder_insert: (LGitTreeEntry of: self repository)
				bld: self
				filename: aString
				id: oidStruct
				filemode: anInteger ]
]

{ #category : #operations }
LGitTreeBuilder >> clear [
	self treebuilder_clear: self
]

{ #category : #operations }
LGitTreeBuilder >> createNew [
	self withReturnHandlerDo: [ 
		self 
			treebuilder_new: self
			repo: self repository
			source: self ffiNull handle ]
]

{ #category : #operations }
LGitTreeBuilder >> getEntryNamed: aString [
	| entry |
	entry := self
		treebuilder_get: self
		filename: aString.
	entry repository: self repository.
	
	^ entry
]

{ #category : #initialization }
LGitTreeBuilder >> initializeWithTree: aTree [
	self withReturnHandlerDo: [
		self
			treebuilder_new: self
			repo: self repository
			source: (aTree ifNotNil: #handle) ]
]

{ #category : #accessing }
LGitTreeBuilder >> newTree [
	^ LGitTree of: self repository fromId: self write
]

{ #category : #'libgit-calls' }
LGitTreeBuilder >> treebuilder_clear: bld [
	
	^ self call: #(void git_treebuilder_clear #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTreeBuilder >> treebuilder_get: bld filename: filename [
	
	^ self
		call: #(LGitTreeEntry git_treebuilder_get #(self , String filename))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTreeBuilder >> treebuilder_insert: out bld: bld filename: filename id: id filemode: filemode [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_treebuilder_insert #(LGitTreeEntry * out , self , String filename , LGitId * id , LGitFilemodeEnum filemode))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTreeBuilder >> treebuilder_new: out repo: repo source: source [
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_treebuilder_new #(LGitTreeBuilder * out , LGitRepository repo , void * source))
		options: #(+ optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitTreeBuilder >> treebuilder_write: id bld: bld [
	
	^ self
		call: #(LGitReturnCodeEnum git_treebuilder_write #(LGitId * id , self))
		options: #()
]

{ #category : #operations }
LGitTreeBuilder >> write [
	| id errorCode |
	id := LGitId new.
	errorCode := self
		treebuilder_write: id
		bld: self.
		
	errorCode ~= 0 ifTrue: [ LGitWritingTreeFailed ].
	
	^ id
]
