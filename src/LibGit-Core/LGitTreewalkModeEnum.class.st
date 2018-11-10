Class {
	#name : #LGitTreewalkModeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitTreewalkModeEnum class >> enumDecl [
	^#(
	GIT_TREEWALK_PRE 0
	GIT_TREEWALK_POST 1
)
]

{ #category : #'accessing-values' }
LGitTreewalkModeEnum class >> git_treewalk_post [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitTreewalkModeEnum class >> git_treewalk_pre [
	^ self basicNew
		value: 0;
		yourself
]
