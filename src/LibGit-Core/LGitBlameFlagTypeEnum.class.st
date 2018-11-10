Class {
	#name : #LGitBlameFlagTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitBlameFlagTypeEnum class >> enumDecl [
	^ #(
		"Normal blame the default"
		GIT_BLAME_NORMAL 0
		"Track lines that have moved within a file (like `git blame -M`).
		 * NOT IMPLEMENTED."
		GIT_BLAME_TRACK_COPIES_SAME_FILE 1
		"Track lines that have moved across files in the same commit (like `git blame -C`).
		 * NOT IMPLEMENTED."
		GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES 2
		"Track lines that have been copied from another file that exists in the
		 * same commit (like `git blame -CC`). Implies SAME_FILE.
		 * NOT IMPLEMENTED."
		GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES 4
		"Track lines that have been copied from another file that exists in *any*
		 * commit (like `git blame -CCC`). Implies SAME_COMMIT_COPIES.
		 * NOT IMPLEMENTED."
		GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES 8
		"Restrict the search of commits to those reachable following only the
		 * first parents."
		GIT_BLAME_FIRST_PARENT 16
	)
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_first_parent [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_normal [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_track_copies_any_commit_copies [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_track_copies_same_commit_copies [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_track_copies_same_commit_moves [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitBlameFlagTypeEnum class >> git_blame_track_copies_same_file [
	^ self basicNew
		value: 1;
		yourself
]
