Class {
	#name : #LGitStructVersionsEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitStructVersionsEnum class >> enumDecl [
	^#(
		GIT_REMOTE_CALLBACKS_VERSION_1 1
	)
]

{ #category : #'accessing-values' }
LGitStructVersionsEnum class >> git_remote_callbacks_version_1 [
	^ self basicNew
		value: 1;
		yourself
]
