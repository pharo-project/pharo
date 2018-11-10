Class {
	#name : #LGitRevwalk,
	#superclass : #LGitRepositoryObject,
	#instVars : [
		'currentCommit',
		'hasNext',
		'sorting'
	],
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'libgit-finalization' }
LGitRevwalk class >> freeFunctionSelector [
	^ #revwalk_free:
]

{ #category : #'instance creation' }
LGitRevwalk class >> of: aRepository [
	^ (super of: aRepository)
		createNew;
		yourself
]

{ #category : #'libgit-calls' }
LGitRevwalk class >> revwalk_free: handle [
	
	^ self call: #(void git_revwalk_free #(void *handle)) options: #()
]

{ #category : #private }
LGitRevwalk >> basicNext [
	"Note: in the C code you would check the return code to see when the loop is over.

	This method uses instances variables to enable more Smalltalk like looping:
	[ walk hasNext ] whileTrue: [ walk next ]."
	| previousCommit nextId |
	previousCommit := currentCommit.
	nextId := self basicNextObjectId.
	currentCommit := self hasNext
		ifTrue: [
			LGitCommit
				of: self repository
				fromId: nextId ]
		ifFalse: [ nil ].
	
	^ previousCommit
]

{ #category : #private }
LGitRevwalk >> basicNextObjectId [
	| nextId returnCode |
	nextId := LGitId new.
	returnCode := self
		revwalk_next: nextId
		walk: self.
	(returnCode ~= LGitReturnCodeEnum git_iterover and: [ 
		returnCode ~= LGitReturnCodeEnum git_ok ]) ifTrue: [ returnCode handleLGitReturnCode ].
	hasNext := returnCode ~= LGitReturnCodeEnum git_iterover.
	
	^ nextId
]

{ #category : #private }
LGitRevwalk >> basicSort: sortingEnum [
	sorting := sorting
		ifNil: [ sortingEnum value ]
		ifNotNil: [ sorting bitOr: sortingEnum value ].
		
	self withReturnHandlerDo: [
		self
			revwalk_sorting: self
			sort_mode: sorting ]
]

{ #category : #operations }
LGitRevwalk >> beSortedByCommitTime [
	self basicSort: LGitRevwalkSortTypeEnum git_sort_time
]

{ #category : #operations }
LGitRevwalk >> beSortedParentsBeforeChildren [
	self basicSort: LGitRevwalkSortTypeEnum git_sort_topological
]

{ #category : #operations }
LGitRevwalk >> beSortedReverse [
	self basicSort: LGitRevwalkSortTypeEnum git_sort_reverse
]

{ #category : #private }
LGitRevwalk >> createNew [
	self withReturnHandlerDo: [
		self
			revwalk_new: self
			repo: self repository ]
]

{ #category : #walking }
LGitRevwalk >> do: aBlock [
	[ self hasNext ] whileTrue: [ 
		aBlock value: self next
	]

]

{ #category : #testing }
LGitRevwalk >> hasNext [
	^ hasNext ifNil: [
		self basicNext.
		hasNext ]
]

{ #category : #operations }
LGitRevwalk >> hideCommit: aLGitId [
	self withReturnHandlerDo: [ self revwalk_hide: aLGitId ]
]

{ #category : #operations }
LGitRevwalk >> hideReference: aReference [
	self withReturnHandlerDo: [
		self
			revwalk_hide_ref: self
			refname: aReference name ]
]

{ #category : #operations }
LGitRevwalk >> next [
	self hasNext ifFalse: [ ^ LGit_GIT_ITEROVER signal ].
	
	^ self basicNext
]

{ #category : #operations }
LGitRevwalk >> pushCommit: aLGitId [ 
	self withReturnHandlerDo: [ self revwalk_push: aLGitId ]
]

{ #category : #operations }
LGitRevwalk >> pushHead [
	self withReturnHandlerDo: [ self revwalk_push_head: self ]
]

{ #category : #operations }
LGitRevwalk >> pushReference: aReference [
	self withReturnHandlerDo: [
		self
			revwalk_push_ref: self
			refname: aReference name ]
]

{ #category : #operations }
LGitRevwalk >> reset [
	self withReturnHandlerDo: [ self revwalk_reset: self ]
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_hide: commitId [
	^ self 
		call:
			#(LGitReturnCodeEnum git_revwalk_hide #(self , LGitId * commitId))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_hide_ref: walk refname: refname [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_revwalk_hide_ref #(self , String refname))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_new: out repo: repo [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_revwalk_new #(LGitRevwalk * self , LGitRepository repo))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_next: out walk: walk [
	
	^ self
		call: #(LGitReturnCodeEnum git_revwalk_next #(LGitId * out , self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_push: commitId [
	^ self 
		call:
			#(LGitReturnCodeEnum git_revwalk_push #(self , LGitId * commitId))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_push_head: walk [
	
	^ self
		call: #(LGitReturnCodeEnum git_revwalk_push_head #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_push_ref: walk refname: refname [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_revwalk_push_ref #(self , String refname))
		options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_reset: revwalk [
	
	^ self call: #(void git_revwalk_reset #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitRevwalk >> revwalk_sorting: walk sort_mode: sort_mode [
	
	^ self
		call: #(void git_revwalk_sorting #(self , uint sort_mode))
		options: #()
]

{ #category : #walking }
LGitRevwalk >> select: selectionBlock thenDo: actionBlock [
	self do: [ :result | 
		(selectionBlock value: result) 
			ifTrue: [ actionBlock value: result ] ]
]
