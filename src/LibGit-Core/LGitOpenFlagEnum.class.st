Class {
	#name : #LGitOpenFlagEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitOpenFlagEnum class >> enumDecl [
	^#(
	GIT_REPOSITORY_OPEN_NO_SEARCH 1
	GIT_REPOSITORY_OPEN_CROSS_FS 2
	GIT_REPOSITORY_OPEN_BARE 4
)
]

{ #category : #'accessing-values' }
LGitOpenFlagEnum class >> git_repository_open_bare [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitOpenFlagEnum class >> git_repository_open_cross_fs [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitOpenFlagEnum class >> git_repository_open_no_search [
	^ self basicNew
		value: 1;
		yourself
]
