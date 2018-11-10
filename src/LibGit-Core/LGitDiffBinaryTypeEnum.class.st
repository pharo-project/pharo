"
/**
 * When producing a binary diff, the binary data returned will be
 * either the deflated full (""literal"") contents of the file, or
 * the deflated binary delta between the two sides (whichever is
 * smaller).
 */
"
Class {
	#name : #LGitDiffBinaryTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffBinaryTypeEnum class >> enumDecl [
	^#(
		" There is no binary delta. "
		GIT_DIFF_BINARY_NONE 0

		" The binary data is the literal contents of the file. "
		GIT_DIFF_BINARY_LITERAL 1

		" The binary data is the delta from one side to the other. "
		GIT_DIFF_BINARY_DELTA 2
	)
]

{ #category : #'accessing-values' }
LGitDiffBinaryTypeEnum class >> git_diff_binary_delta [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffBinaryTypeEnum class >> git_diff_binary_literal [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffBinaryTypeEnum class >> git_diff_binary_none [
	^ self basicNew
		value: 0;
		yourself
]
