Class {
	#name : #LGitDiffFormatTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffFormatTypeEnum class >> enumDecl [
	^#(
	GIT_DIFF_FORMAT_PATCH        1 "full git diff"
	GIT_DIFF_FORMAT_PATCH_HEADER 2 "just the file headers of patch"
	GIT_DIFF_FORMAT_RAW          3 "like git diff --raw"
	GIT_DIFF_FORMAT_NAME_ONLY    4 "like git diff --name-only"
	GIT_DIFF_FORMAT_NAME_STATUS  5 "like git diff --name-status"
	)
]

{ #category : #'accessing-values' }
LGitDiffFormatTypeEnum class >> git_diff_format_name_only [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFormatTypeEnum class >> git_diff_format_name_status [
	^ self basicNew
		value: 5;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFormatTypeEnum class >> git_diff_format_patch [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFormatTypeEnum class >> git_diff_format_patch_header [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffFormatTypeEnum class >> git_diff_format_raw [
	^ self basicNew
		value: 3;
		yourself
]
