Class {
	#name : #LGitIndex,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-finalization' }
LGitIndex class >> freeFunctionSelector [
	^ #index_free:
]

{ #category : #'libgit-calls' }
LGitIndex class >> index_free: handle [
	
	^ self call: #(void git_index_free #(void *handle)) options: #()
]

{ #category : #operations }
LGitIndex >> addAll [
	| pathspec |
	pathspec := LGitStringArray withAll: #().
	self withReturnHandlerDo: [ 
		self
			index_add_all: self
			pathspec: pathspec
			flags: 0
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: ExternalAddress null ]
]

{ #category : #operations }
LGitIndex >> addAll: pathSpecs [
	| pathspec |
	pathspec := LGitStringArray withAll: pathSpecs.
	self withReturnHandlerDo: [ 
		self
			index_add_all: self
			pathspec: pathspec
			flags: 0
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: ExternalAddress null ]

]

{ #category : #enumerating }
LGitIndex >> allFilesStatusDo: aBlock [
	"The callback is passed the path of the file, the status (a combination of the git_status_t values above) and the payload data pointer passed into this function.

If the callback returns a non-zero value, this function will stop looping and return that value to caller."
	self withReturnHandlerDo: [ 
		self
			status_foreach: self repository
			callback: (LGitStatusCallback on: aBlock)
			payload: nil ]
]

{ #category : #operations }
LGitIndex >> checkout [
	self withReturnHandlerDo: [ 
		self checkout_index_withOptions: 
			(LGitCheckoutOptions defaults
				checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_force;
				yourself) ]
]

{ #category : #'libgit-calls' }
LGitIndex >> checkout_index_withOptions: opts [
	^ self
		call: #(LGitReturnCodeEnum 
			git_checkout_index(
				LGitRepository repository, 
				self, 
				LGitCheckoutOptions *opts))
		options: #()

]

{ #category : #enumerating }
LGitIndex >> entriesSelect: aBlock [
	| entries |
	entries := OrderedCollection new.
	self allFilesStatusDo: [ :path :status :payload |
		(aBlock value: path value: status) ifTrue: [ entries add: {path. status. payload} ].
		0 ].
	
	^ entries
]

{ #category : #operations }
LGitIndex >> entryCount [
	^ self index_entrycount: self
]

{ #category : #operations }
LGitIndex >> forceRefresh [
	self withReturnHandlerDo: [ 
		self
			index_read: self
			force: true ]
]

{ #category : #hack }
LGitIndex >> hackOwner: aRepository [
	"There is a bug in version 0.23 of libgit2, the returned index does not have an 
	 owner (a pointer to a repository), as you would expect from the documentation. 
	This breaks libgit encapsulation, and will probably fail if updating to a newer 
	version, but anyway if we migrate this bug should be fixed."
	self flag: #fixMe. "When possible :)"
	"
	NOTE: is Smalltalk wordSize + 1 because the structure is something like: 
	struct git_index {
		git_refcount rc;
 		...
	}
	
	then:
	
	struct git_refcount { 
		git_atomic refcount;
		void *owner;
	}
	
	and finally: 
	git_atomic = int -> 32bits
	git_atomic = int64_t -> 64bits
	
	So we need to ensure the owner is set properly.
	"
	self handle 
		pointerAt: Smalltalk wordSize + 1
		put: repository handle
]

{ #category : #testing }
LGitIndex >> hasConflicts [
	^ self index_has_conflicts asBoolean
]

{ #category : #operations }
LGitIndex >> hasEntry: aString [
	^ self withReturnHandlerDo: [ 
		self
			index_find: nil
			index: self
			path: aString ]
]

{ #category : #'libgit-calls' }
LGitIndex >> index_add_all: index pathspec: pathspec flags: flags callback: callback payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_add_all #(
				self, 
				LGitStringArray *pathspec, 
				uint flags, 
				LGitIndexMatchedPathCallback callback, 
				void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitIndex >> index_entrycount: index [
	
	^ self call: #(size_t git_index_entrycount #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_find: at_pos index: index path: path [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_find #(LGitWriteBackValueSizeT *at_pos , self, String path))
		options: #(optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitIndex >> index_has_conflicts [ 
	^ self call: #(LGitReturnCodeEnum git_index_has_conflicts #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_owner [
	^ self call: #(LGitRepository * git_index_owner (LGitIndex self)) options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_path [
	^ self
		call: #(String git_index_path #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_read: index force: force [
	
	^ self
		call: #(LGitReturnCodeEnum git_index_read #(self , Boolean force))
		options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_remove_all: index pathspec: pathspec callback: callback payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_remove_all #(self, LGitStringArray * pathspec, LGitIndexMatchedPathCallback callback, void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitIndex >> index_update_all: index pathspec: pathspec callback: callback payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_update_all #(self, LGitStringArray *pathspec, LGitIndexMatchedPathCallback callback, void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitIndex >> index_write [
	^ self
		call: #(LGitReturnCodeEnum git_index_write #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> index_write_tree: out index: index [
	
	^ self
		call: #(LGitReturnCodeEnum git_index_write_tree #(LGitId * out , self))
		options: #()
]

{ #category : #testing }
LGitIndex >> isBeingTracked: aString [
	| intValue |
	(self repository repositoryReference resolve: aString) isDirectory ifTrue: [ ^ true ].
	
	intValue := LGitWriteBackValueUInt new.
	self withReturnHandlerDo: [ 
		self
			status_file: intValue
			repo: self repository
			path: aString ].

	^ (LGitStatusTypeEnum  git_status_wt_new isPartOfBitFlag: intValue value) not
]

{ #category : #testing }
LGitIndex >> isDirty: aFileReference [
	| intValue |
	intValue := LGitWriteBackValueInt new.
	self withReturnHandlerDo: [
		self
			status_file: intValue
			repo: self repository
			path: ((aFileReference relativeTo: self repository repositoryReference) printWithDelimiter: $/) ].
		
	^ intValue value isZero not
]

{ #category : #accessing }
LGitIndex >> owner [
	^ self index_owner
]

{ #category : #accessing }
LGitIndex >> path [
	^ self index_path
]

{ #category : #operations }
LGitIndex >> removeAll [
	| pathspec |
	pathspec := LGitStringArray withAll: #().
	self withReturnHandlerDo: [ 
		self
			index_remove_all: self
			pathspec: pathspec
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: nil ]
]

{ #category : #'libgit-calls' }
LGitIndex >> status_file: status_flags repo: repo path: path [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_status_file #(LGitWriteBackValueUInt status_flags, LGitRepository repo, String path))
		options: #()
]

{ #category : #'libgit-calls' }
LGitIndex >> status_foreach: repo callback: callback payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_status_foreach #(LGitRepository repo, LGitStatusCallback callback, void * payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #operations }
LGitIndex >> updateAll [
	| pathspec |
	pathspec := LGitStringArray withAll: { '*' }.
	self withReturnHandlerDo: [ 
		self
			index_update_all: self
			pathspec: pathspec
			callback: (LGitIndexMatchedPathCallback on: [ :a :b :c | 0 ])
			payload: nil ]
]

{ #category : #operations }
LGitIndex >> writeToDisk [
	self withReturnHandlerDo: [ self index_write ]
]

{ #category : #operations }
LGitIndex >> writeTreeAndReturnOid [
	| oid |
	oid := LGitId externalNew.
	self withReturnHandlerDo: [ 
		self
			index_write_tree: oid
			index: self ].
		
	^ oid
]
