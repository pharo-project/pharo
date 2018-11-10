Class {
	#name : #LGitSubmoduleIgnoreTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitSubmoduleIgnoreTypeEnum class >> enumDecl [
	^#(
	GIT_SUBMODULE_IGNORE_RESET     -1 "reset to on-disk value"

	GIT_SUBMODULE_IGNORE_NONE      1  "any change or untracked == dirty"
	GIT_SUBMODULE_IGNORE_UNTRACKED 2  "dirty if tracked files change"
	GIT_SUBMODULE_IGNORE_DIRTY     3  "only dirty if HEAD moved"
	GIT_SUBMODULE_IGNORE_ALL       4  "never dirty"

	GIT_SUBMODULE_IGNORE_DEFAULT   0
)
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_all [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_default [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_dirty [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_none [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_reset [
	^ self basicNew
		value: -1;
		yourself
]

{ #category : #'accessing-values' }
LGitSubmoduleIgnoreTypeEnum class >> git_submodule_ignore_untracked [
	^ self basicNew
		value: 2;
		yourself
]
