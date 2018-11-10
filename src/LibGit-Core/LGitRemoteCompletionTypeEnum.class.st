Class {
	#name : #LGitRemoteCompletionTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitRemoteCompletionTypeEnum class >> enumDecl [
	^#(
	GIT_REMOTE_COMPLETION_DOWNLOAD 0
	GIT_REMOTE_COMPLETION_INDEXING 1
	GIT_REMOTE_COMPLETION_ERROR 2
)
]

{ #category : #'accessing-values' }
LGitRemoteCompletionTypeEnum class >> git_remote_completion_download [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitRemoteCompletionTypeEnum class >> git_remote_completion_error [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitRemoteCompletionTypeEnum class >> git_remote_completion_indexing [
	^ self basicNew
		value: 1;
		yourself
]
