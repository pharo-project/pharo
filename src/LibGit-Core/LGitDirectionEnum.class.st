Class {
	#name : #LGitDirectionEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDirectionEnum class >> enumDecl [
	^#(
	GIT_DIRECTION_FETCH 0
	GIT_DIRECTION_PUSH 1
)
]

{ #category : #'accessing-values' }
LGitDirectionEnum class >> git_direction_fetch [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitDirectionEnum class >> git_direction_push [
	^ self basicNew
		value: 1;
		yourself
]
