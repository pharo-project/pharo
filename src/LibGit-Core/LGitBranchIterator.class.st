Class {
	#name : #LGitBranchIterator,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'instance creation' }
LGitBranchIterator class >> allBranchesOf: repository do: aBlock [
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_all
		do: aBlock
]

{ #category : #'libgit-calls' }
LGitBranchIterator class >> branch_iterator_free: handle [
	
	^ self
		call: #(void git_branch_iterator_free #(void *handle))
		options: #()
]

{ #category : #'native-boost' }
LGitBranchIterator class >> freeFunctionSelector [
	^ #branch_iterator_free:
]

{ #category : #'instance creation' }
LGitBranchIterator class >> localBranchesOf: repository do: aBlock [
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_local
		do: aBlock
]

{ #category : #'instance creation' }
LGitBranchIterator class >> of: aRepository branchType: aLGitBranchTypeEnum do: aBlock [
	^ (self of: aRepository)
		iterate: aLGitBranchTypeEnum;
		branchesDo: aBlock
]

{ #category : #'instance creation' }
LGitBranchIterator class >> remoteBranchesOf: repository do: aBlock [
	self 
		of: repository 
		branchType: LGitBranchTypeEnum git_branch_remote
		do: aBlock
]

{ #category : #'libgit-calls' }
LGitBranchIterator >> branch_iterator_new: out repo: repo list_flags: list_flags [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_branch_iterator_new #(LGitBranchIterator * self , LGitRepository repo , LGitBranchTypeEnum list_flags))
		options: #()
]

{ #category : #'libgit-calls' }
LGitBranchIterator >> branch_next: out out_type: out_type [
	^ self
		call:
			#(LGitReturnCodeEnum git_branch_next #(
				LGitReference * out ,
				LGitWriteBackValueUInt * out_type,
				LGitBranchIterator self))
		options: #()
]

{ #category : #operations }
LGitBranchIterator >> branchesDo: aBlock [
	| returnCode branchType reference finishedCode isLocal |
	
	returnCode := LGitReturnCodeEnum git_ok.
	finishedCode := LGitReturnCodeEnum git_iterover.
	branchType := LGitWriteBackValueUInt new.
	
	[ 
		reference := LGitReference of: self repository.
		returnCode := self branch_next: reference out_type: branchType.
		isLocal := branchType value = LGitBranchTypeEnum git_branch_local value.
		returnCode = finishedCode 
	] 
	whileFalse: [ aBlock cull: reference cull: isLocal ]
]

{ #category : #operations }
LGitBranchIterator >> iterate: aLGitBranchTypeEnum [
	self repository signalIfNotReady.
	self withReturnHandlerDo: [
		self
			branch_iterator_new: self
			repo: self repository
			list_flags: aLGitBranchTypeEnum ]
]
