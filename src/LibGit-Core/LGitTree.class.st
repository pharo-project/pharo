Class {
	#name : #LGitTree,
	#superclass : #LGitObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'accessing-defaults' }
LGitTree class >> defaultFilemode [
	^ LGitFilemodeEnum git_filemode_tree
]

{ #category : #'libgit-finalization' }
LGitTree class >> freeFunctionSelector [
	^ #tree_free:
]

{ #category : #testing }
LGitTree class >> isType: aType [
	^ aType = LGitObjectTypeEnum git_obj_tree
]

{ #category : #'libgit-calls' }
LGitTree class >> tree_free: handle [
	
	^ self call: #(void git_tree_free #(void *handle)) options: #()
]

{ #category : #comparing }
LGitTree >> = anObject [
	^ self == anObject or: [ 
		self class == anObject class and: [ 
			self isReady and: [
				anObject isReady and: [
					self id = anObject id ] ] ] ]
]

{ #category : #enumerating }
LGitTree >> allEntriesDo: aBlock [
	"The entries will be traversed in the specified order, children subtrees will be automatically loaded as required, and the callback will be called once per entry with the current (relative) root for the entry and the entry data itself.

If the callback returns a positive value, the passed entry will be skipped on the traversal (in pre mode). A negative value stops the walk."
	self withReturnHandlerDo: [ 
		self
			tree_walk: self
			mode: LGitTreewalkModeEnum git_treewalk_pre
			callback: (LGitTreewalkCallback on: aBlock)
			payload: ExternalAddress null ]
]

{ #category : #accessing }
LGitTree >> diffTo: aTree [
	| diff |
	diff := LGitDiff of: self repository.
	diff 
		diffTree: self 
		toTree: aTree.
	^ diff
]

{ #category : #accessing }
LGitTree >> diffTo: aTree options: options [
	| diff |
	diff := LGitDiff of: self repository.
	diff 
		diffTree: self 
		toTree: aTree
		options: options.
	^ diff
]

{ #category : #traversing }
LGitTree >> entries [
	| entryCount |
	entryCount := self tree_entrycount: self.
	
	^ Array new: entryCount streamContents: [ :stream |
		0 to: entryCount - 1 do: [ :i || entry |
			entry := self tree_entry_byindex: self idx: i.
			entry parent: self.
			entry repository: self repository.
			stream nextPut: entry ] ]
]

{ #category : #accessing }
LGitTree >> entryByName: aString [ 
	^ self entryByName: aString ifAbsent: [ 
		self error: ('Entry {1} is not present in tree {2}' format: {aString. self})].

]

{ #category : #accessing }
LGitTree >> entryByName: aString ifAbsent: aBlock [
	| entry |
	entry := self tree_entry_byname: self name: aString.
	(entry isNil or: [ entry isNull ])
		ifTrue: [ ^aBlock value ].
	entry repository: self repository.
	^entry
]

{ #category : #accessing }
LGitTree >> entryByPath: aString [ 
	^ self entryByPath: aString ifAbsent: [ 
		self error: ('Entry {1} is not present in tree {2}' format: {aString. self})].

]

{ #category : #accessing }
LGitTree >> entryByPath: aString ifAbsent: aBlock [
    ^ self 
        entryByRelativePath: (self relativePath: aString)
        ifAbsent: aBlock

]

{ #category : #accessing }
LGitTree >> entryByRelativePath: aString ifAbsent: aBlock [
    | entry |
    entry := (LGitTreeEntry of: self repository) autoRelease.
    self tree_entry_bypath: entry path: aString.
    ^ entry isNull
        ifTrue: [ aBlock value ]
        ifFalse: [ 
            entry repository: self repository.
            entry ]

]

{ #category : #private }
LGitTree >> get_id_data [
	^ self tree_id: self
]

{ #category : #comparing }
LGitTree >> hash [
	^ super hash bitXor: self id asInteger
]

{ #category : #testing }
LGitTree >> isEmpty [
	^ self entries isEmpty
]

{ #category : #testing }
LGitTree >> isLeaf [
	^ self entries noneSatisfy: [ :entry |
		entry type = LGitObjectTypeEnum git_obj_tree  ]
]

{ #category : #testing }
LGitTree >> isTree [
	^ true
]

{ #category : #private }
LGitTree >> lookup [
	self withReturnHandlerDo: [
		self
			tree_lookup: self
			repo: self repository
			id: self id ].
	self autoRelease
]

{ #category : #accessing }
LGitTree >> matchesPathSpec: aPathSpec [
	^ (self 
		pathspec_match: aPathSpec 
		flags: LGitPathSpecFlagTypeEnum git_pathspec_no_match_error) == 0
]

{ #category : #accessing }
LGitTree >> names [
	<todo: 'decide what to do with this:
	- will not work for trees not referenced from head (e.g. in an older commit)
	- very slow for large repositories because every entry is visited'>
	| names |
	names := OrderedCollection new.
	self repository head object tree allEntriesDo: [ :name :entry :payload |
		entry repository: self repository.
		entry object = self ifTrue: [ names add: name ].
		0 ].
	
	^ names
]

{ #category : #printing }
LGitTree >> objectPrintOn: aStream [
	aStream
		nextPutAll: 'number of entries: ';
		nextPutAll: self size asString
]

{ #category : #accessing }
LGitTree >> owner [
	^ self tree_owner: self
]

{ #category : #'libgit-calls' }
LGitTree >> pathspec_match: ps flags: flags [
	^ self call: #(int git_pathspec_match_tree(0, self, LGitPathSpecFlagTypeEnum flags, LGitPathSpec ps))
]

{ #category : #accessing }
LGitTree >> relativePath: aString [
    "remove leading / characters"
        ^ aString trimLeft: [ :c | c = $/ ]
]

{ #category : #accessing }
LGitTree >> size [
	^ self tree_entrycount: self
]

{ #category : #traversing }
LGitTree >> subTreeEntries [
	^ self entries select: [ :entry |
		entry type = LGitObjectTypeEnum git_obj_tree ]
]

{ #category : #'libgit-calls' }
LGitTree >> tree_entry_byindex: tree idx: idx [
	
	^ self
		call: #(LGitTreeEntry git_tree_entry_byindex #(self , size_t idx))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_entry_byname: tree name: name [
	
	^ self
		call: #(LGitTreeEntry git_tree_entry_byname #(self , String name))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_entry_bypath: out path: path [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_entry_bypath #(LGitTreeEntry * out, self, String path))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_entrycount: tree [
	
	^ self call: #(int git_tree_entrycount #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_id: tree [
	
	^ self call: #(void * git_tree_id #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_lookup: out repo: repo id: objectId [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_tree_lookup #(LGitTree * out , LGitRepository repo , LGitId * objectId))
		options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_owner: tree [
	
	^ self call: #(LGitRepository git_tree_owner #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTree >> tree_walk: tree mode: mode callback: callback payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_walk #(LGitTree tree , LGitTreewalkModeEnum mode , LGitTreewalkCallback callback , void *payload))
		options: #(optMayGC optCoerceNilToNull)
]
