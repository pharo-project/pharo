"
/**
 * Status flags for a single file.
 *
 * A combination of these values will be returned to indicate the status of
 * a file.  Status compares the working directory, the index, and the
 * current HEAD of the repository.  The `GIT_STATUS_INDEX` set of flags
 * represents the status of file in the index relative to the HEAD, and the
 * `GIT_STATUS_WT` set of flags represent the status of the file in the
 * working directory relative to the index.
 */
"
Class {
	#name : #LGitStatusTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitStatusTypeEnum class >> enumDecl [
	^#(
		GIT_STATUS_CURRENT  0
		GIT_STATUS_INDEX_NEW         "1 bitShift: 0" 1
		GIT_STATUS_INDEX_MODIFIED    "1 bitShift: 1" 2
		GIT_STATUS_INDEX_DELETED     "1 bitShift: 2" 4
		GIT_STATUS_INDEX_RENAMED     "1 bitShift: 3" 8
		GIT_STATUS_INDEX_TYPECHANGE  "1 bitShift: 4" 16
		GIT_STATUS_WT_NEW            "1 bitShift: 7" 128
		GIT_STATUS_WT_MODIFIED       "1 bitShift: 8" 256
		GIT_STATUS_WT_DELETED        "1 bitShift: 9" 512
		GIT_STATUS_WT_TYPECHANGE     "1 bitShift: 10" 1024
		GIT_STATUS_WT_RENAMED        "1 bitShift: 11" 2048
		GIT_STATUS_WT_UNREADABLE     "1 bitShift: 12" 4096
		GIT_STATUS_IGNORED           "1 bitShift: 14" 16384
		GIT_STATUS_CONFLICTED        "1 bitShift: 15" 32768
)
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_conflicted [
	^ self basicNew
		value: 32768;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_current [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_ignored [
	^ self basicNew
		value: 16384;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_index_deleted [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_index_modified [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_index_new [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_index_renamed [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_index_typechange [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_deleted [
	^ self basicNew
		value: 512;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_modified [
	^ self basicNew
		value: 256;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_new [
	^ self basicNew
		value: 128;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_renamed [
	^ self basicNew
		value: 2048;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_typechange [
	^ self basicNew
		value: 1024;
		yourself
]

{ #category : #'accessing-values' }
LGitStatusTypeEnum class >> git_status_wt_unreadable [
	^ self basicNew
		value: 4096;
		yourself
]
