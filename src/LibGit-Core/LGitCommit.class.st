"
A LGitCommit represents a specific commit and the associate information.

Instance Variables
	handle:		NBExternalObject

handle
	- the handle to the external data object

"
Class {
	#name : #LGitCommit,
	#superclass : #LGitObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-calls' }
LGitCommit class >> commit_free: handle [
	
	^ self call: #(void git_commit_free #(void *handle)) options: #()
]

{ #category : #'libgit-finalization' }
LGitCommit class >> freeFunctionSelector [
	^ #commit_free:
]

{ #category : #testing }
LGitCommit class >> isType: aType [
	^ aType = LGitObjectTypeEnum git_obj_commit
]

{ #category : #'instance creation' }
LGitCommit class >> of: aRepository fromHexString: aCommitId [
	^ self of: aRepository fromId: (LGitId fromHexString: aCommitId)
]

{ #category : #comparing }
LGitCommit >> = anObject [
	^ self == anObject or: [ 
		self class == anObject class and: [
			self isReady and: [
				anObject isReady and: [
					self id = anObject id ] ] ] ]
]

{ #category : #accessing }
LGitCommit >> author [
	^ self commit_author: self
]

{ #category : #accessing }
LGitCommit >> authorStruct [
	^ self commit_author: self
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_author: commit [
	
	^ self
		call: #(LGitSignature * git_commit_author #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_committer: commit [
	
	^ self
		call: #(LGitSignature * git_commit_committer #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_id: commit [
	
	^ self call: #(void * git_commit_id #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_lookup: commit repo: repo id: objectId [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_commit_lookup #(LGitCommit * commit , LGitRepository repo , LGitId * objectId))
		options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_message: commit [
	
	^ self call: #(String git_commit_message #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_message_encoding: commit [
	
	^ self
		call: #(String git_commit_message_encoding #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_parent_id: commit n: n [
	<todo: 'using LGitId * as return type causes a crash'>
	
	^ self
		call: #(void *git_commit_parent_id #(self , uint n))
		options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_parentcount: commit [
	
	^ self call: #(uint git_commit_parentcount #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_raw_header: aCommit [
	^ self call: #(const char * git_commit_raw_header(self))
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_time: commit [
	
	^ self call: #(int64 git_commit_time #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_time_offset: commit [
	
	^ self call: #(int git_commit_time_offset #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitCommit >> commit_tree: tree_out commit: commit [
	
	^ self
		call: #(LGitReturnCodeEnum git_commit_tree #(LGitTree *tree_out , self))
		options: #()
]

{ #category : #accessing }
LGitCommit >> committer [
	^ self commit_committer: self
]

{ #category : #accessing }
LGitCommit >> committerStruct [
	^ self commit_committer: self
]

{ #category : #accessing }
LGitCommit >> entryByPath: aString [ 
	^ self tree entryByPath: aString

]

{ #category : #accessing }
LGitCommit >> entryByPath: aString ifAbsent: aBlock [
	^ self tree entryByPath: aString ifAbsent: aBlock

]

{ #category : #private }
LGitCommit >> get_id_data [
	^ self commit_id: self
]

{ #category : #testing }
LGitCommit >> hasParents [
	^ self parents notEmpty
]

{ #category : #testing }
LGitCommit >> isCommit [
	^ true
]

{ #category : #testing }
LGitCommit >> isDescendantOf: aCommit [
	^ (self repo: self repository graph: self id descendant_of: aCommit id) == 1
]

{ #category : #testing }
LGitCommit >> isMerge [
	^ self parents size > 1
]

{ #category : #private }
LGitCommit >> lookup [
	self withReturnHandlerDo: [ 
		self
			commit_lookup: self
			repo: self repository
			id: self id ].
	self autoRelease
]

{ #category : #operations }
LGitCommit >> merge: anotherCommit [
	"Merge two commits, producing a git_index that reflects the result of the merge."
	| index | 
	index := LGitIndex of: self repository.
	self withReturnHandlerDo: [ 
		self merge_result: index commit: anotherCommit options: LGitMergeOptions defaults.
	].

	"There is a bug on libgit2 and we need to set the owner of the index (because it is not
	setting it)"
	index hackOwner: self repository.
	
	^ index

]

{ #category : #'libgit-calls' }
LGitCommit >> merge_result: out commit: anotherCommit options: options [
	^ self call: #(
		LGitReturnCodeEnum git_merge_commits(
			LGitIndex *out, 
			LGitRepository repository, 
			LGitCommit self, 
			LGitCommit anotherCommit, 
			LGitMergeOptions *options))
		options: #()
]

{ #category : #accessing }
LGitCommit >> message [
	<todo: 'use encoding to properly read the message'>
	|  encoding |
	encoding := self commit_message_encoding: self.
	^ self commit_message: self
]

{ #category : #accessing }
LGitCommit >> name [
	
	^ id hexString
]

{ #category : #accessing }
LGitCommit >> numberOfParents [
	^ self commit_parentcount: self
]

{ #category : #printing }
LGitCommit >> objectPrintOn: aStream [
	self id hexString in: [ :hexId |
		aStream
			nextPutAll: (hexId first: 7);
			nextPutAll: '...';
			nextPutAll: (hexId last: 7);
			nextPutAll: ', ';
			nextPutAll: self message surroundedBySingleQuotes ]
]

{ #category : #accessing }
LGitCommit >> owner [
	^ self 
		call: #(void *git_commit_owner(self)) 
		options: #()

]

{ #category : #accessing }
LGitCommit >> parents [
	| parentCount |
	parentCount := self commit_parentcount: self.
	^ Array
		new: parentCount
		streamContents: [ :stream | 
			0 to: parentCount - 1 do: [ :i | 
				| parentOid parent |
				parentOid := LGitId fromHandle: (self commit_parent_id: self n: i) getHandle.
				parent := self class of: self repository fromId: parentOid.
				stream nextPut: parent ] ]
]

{ #category : #accessing }
LGitCommit >> rawHeader [ 
	^ self commit_raw_header: self
]

{ #category : #'libgit-calls' }
LGitCommit >> repo: repo graph: commit descendant_of: ancestor [
	^ self 
		call: #(int git_graph_descendant_of(LGitRepository repo, LGitId *commit, LGitId *ancestor))
		options: #() 
]

{ #category : #accessing }
LGitCommit >> time [
	| time offset |
	time := self commit_time: self.
	offset := self commit_time_offset: self.
	^ LGitTime new
		time: time;
		offset: offset;
		yourself
]

{ #category : #accessing }
LGitCommit >> tree [
	| tree |
	tree := LGitTree of: self repository.
	self commit_tree: tree commit: self.
	^ tree
]
