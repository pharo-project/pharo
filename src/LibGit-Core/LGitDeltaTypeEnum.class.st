"
/**
 * What type of change is described by a git_diff_delta?
 *
 * `GIT_DELTA_RENAMED` and `GIT_DELTA_COPIED` will only show up if you run
 * `git_diff_find_similar()` on the diff object.
 *
 * `GIT_DELTA_TYPECHANGE` only shows up given `GIT_DIFF_INCLUDE_TYPECHANGE`
 * in the option flags (otherwise type changes will be split into ADDED /
 * DELETED pairs).
 */
"
Class {
	#name : #LGitDeltaTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDeltaTypeEnum class >> enumDecl [
	^#(
	GIT_DELTA_UNMODIFIED 0 "no changes"
	GIT_DELTA_ADDED 1 "entry does not exist in old version"
	GIT_DELTA_DELETED 2 "entry does not exist in new version"
	GIT_DELTA_MODIFIED 3 "entry content changed between old and new"
	GIT_DELTA_RENAMED 4 "entry was renamed between old and new"
	GIT_DELTA_COPIED 5 "entry was copied from another old entry"
	GIT_DELTA_IGNORED 6 "entry is ignored item in workdir"
	GIT_DELTA_UNTRACKED 7 "entry is untracked item in workdir"
	GIT_DELTA_TYPECHANGE 8 "type of entry changed between old and new"
	GIT_DELTA_UNREADABLE 9 "entry is unreadable"
	GIT_DELTA_CONFLICTED 10 "< entry in the index is conflicted"
)
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_added [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_conflicted [
	^ self basicNew
		value: 10;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_copied [
	^ self basicNew
		value: 5;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_deleted [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_ignored [
	^ self basicNew
		value: 6;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_modified [
	^ self basicNew
		value: 3;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_renamed [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_typechange [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_unmodified [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_unreadable [
	^ self basicNew
		value: 9;
		yourself
]

{ #category : #'accessing-values' }
LGitDeltaTypeEnum class >> git_delta_untracked [
	^ self basicNew
		value: 7;
		yourself
]
