Class {
	#name : #LGitResetTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitResetTypeEnum class >> enumDecl [
	^ #(
		GIT_RESET_SOFT  1 "Move the head to the given commit"
		GIT_RESET_MIXED 2 "SOFT plus reset index to the commit"
		GIT_RESET_HARD  3 "MIXED plus changes in working tree discarded"
	)
]

{ #category : #'accessing-values' }
LGitResetTypeEnum class >> git_reset_hard [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitResetTypeEnum class >> git_reset_mixed [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitResetTypeEnum class >> git_reset_soft [
	^ self basicNew
		value: 1;
		yourself
]
