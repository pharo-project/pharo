Class {
	#name : #LGitFetchPruneTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitFetchPruneTypeEnum class >> enumDecl [
	^ #(
		"
		 * Use the setting from the configuration
		 "
		GIT_FETCH_PRUNE_UNSPECIFIED 0
		"
		 * Force pruning on
		 "
		GIT_FETCH_PRUNE 1
		"
		 * Force pruning off
		 "
		GIT_FETCH_NO_PRUNE 2
	)
]

{ #category : #'accessing-values' }
LGitFetchPruneTypeEnum class >> git_fetch_no_prune [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitFetchPruneTypeEnum class >> git_fetch_prune [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitFetchPruneTypeEnum class >> git_fetch_prune_unspecified [
	^ self basicNew
		value: 0;
		yourself
]
