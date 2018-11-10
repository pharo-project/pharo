Class {
	#name : #LGitBranchTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitBranchTypeEnum class >> enumDecl [
	^#(
	GIT_BRANCH_LOCAL 1
	GIT_BRANCH_REMOTE 2
	GIT_BRANCH_ALL 3
)
]

{ #category : #'accessing-values' }
LGitBranchTypeEnum class >> git_branch_all [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitBranchTypeEnum class >> git_branch_local [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitBranchTypeEnum class >> git_branch_remote [
	^ self basicNew
		value: 2;
		yourself
]
