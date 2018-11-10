"
I map names to versions for all the option structs.
"
Class {
	#name : #LGitOptionsVersionsEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitOptionsVersionsEnum class >> enumDecl [
	^#(
	GIT_BLAME_OPTIONS_VERSION_1 1
	GIT_CHECKOUT_OPTIONS_VERSION_1 1
	GIT_CLONE_OPTIONS_VERSION_1 1
	GIT_DIFF_OPTIONS_VERSION_1 1
	GIT_DIFF_FIND_OPTIONS_VERSION_1 1
	GIT_FETCH_OPTIONS_VERSION_1 1
	GIT_PUSH_OPTIONS_VERSION_1 1
	)
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_blame_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_checkout_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_clone_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_diff_find_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_diff_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_fetch_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_merge_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitOptionsVersionsEnum class >> git_push_options_version_1 [
	^ self basicNew
		value: 1;
		yourself
]
