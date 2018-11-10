Class {
	#name : #LGitDiffFlagTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffFlagTypeEnum class >> enumDecl [
	^#(
	GIT_DIFF_FLAG_BINARY 0 "file(s) treated as binary data"
	GIT_DIFF_FLAG_NOT_BINARY 1 "file(s) treated as text data"
	GIT_DIFF_FLAG_VALID_ID  2 "`id` value is known correct"
	)
]

{ #category : #'accessing-values' }
LGitDiffFlagTypeEnum class >> git_diff_flag_binary [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFlagTypeEnum class >> git_diff_flag_not_binary [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFlagTypeEnum class >> git_diff_flag_valid_id [
	^ self basicNew
		value: 2;
		yourself
]
