Class {
	#name : #LGitDiffLineTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffLineTypeEnum class >> enumDecl [
	^{
	 "These values will be sent to `git_diff_line_cb` along with the line"
	#GIT_DIFF_LINE_CONTEXT.   '$ '.
	#GIT_DIFF_LINE_ADDITION.  '$+'.
	#GIT_DIFF_LINE_DELETION.  '$-'.

	#GIT_DIFF_LINE_CONTEXT_EOFNL. '$='."Both files have no LF at end"
	#GIT_DIFF_LINE_ADD_EOFNL. '$>'.    "Old has no LF at end new does"
	#GIT_DIFF_LINE_DEL_EOFNL. '$<'.    "Old has LF at end new does not"

	"The following values will only be sent to a `git_diff_line_cb` when
	 * the content of a diff is being formatted through `git_diff_print`.
	"
	#GIT_DIFF_LINE_FILE_HDR.  '$F'.
	#GIT_DIFF_LINE_HUNK_HDR.  '$H'.
	#GIT_DIFF_LINE_BINARY.    '$B' "For 'Binary files x and y differ'"
	}
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_add_eofnl [
	^ self basicNew
		value: $>;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_addition [
	^ self basicNew
		value: $+;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_binary [
	^ self basicNew
		value: $B;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_context [
	^ self basicNew
		value: $ ;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_context_eofnl [
	^ self basicNew
		value: $=;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_del_eofnl [
	^ self basicNew
		value: $<;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_deletion [
	^ self basicNew
		value: $-;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_file_hdr [
	^ self basicNew
		value: $F;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffLineTypeEnum class >> git_diff_line_hunk_hdr [
	^ self basicNew
		value: $H;
		yourself
]
