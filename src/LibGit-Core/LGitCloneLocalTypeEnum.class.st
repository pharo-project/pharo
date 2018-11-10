Class {
	#name : #LGitCloneLocalTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitCloneLocalTypeEnum class >> enumDecl [
	^ #(
			GIT_CLONE_LOCAL_AUTO 0
			GIT_CLONE_LOCAL 1
			GIT_CLONE_NO_LOCAL 2
			GIT_CLONE_LOCAL_NO_LINKS 3
		)
]

{ #category : #'accessing-values' }
LGitCloneLocalTypeEnum class >> git_clone_local [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitCloneLocalTypeEnum class >> git_clone_local_auto [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitCloneLocalTypeEnum class >> git_clone_local_no_links [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitCloneLocalTypeEnum class >> git_clone_no_local [
	^ self basicNew
		value: 2;
		yourself
]
