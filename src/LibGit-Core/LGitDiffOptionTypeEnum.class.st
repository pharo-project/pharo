Class {
	#name : #LGitDiffOptionTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffOptionTypeEnum class >> enumDecl [
	^#(
	"Normal diff, the default"
	GIT_DIFF_NORMAL 0

	"
	 * Options controlling which files will be in the diff
	"

	"Reverse the sides of the diff"
	GIT_DIFF_REVERSE 1

	"Include ignored files in the diff"
	GIT_DIFF_INCLUDE_IGNORED 2

	"Even with GIT_DIFF_INCLUDE_IGNORED, an entire ignored directory
	 *  will be marked with only a single entry in the diff; this flag
	 *  adds all files under the directory as IGNORED entries, too.
	"
	GIT_DIFF_RECURSE_IGNORED_DIRS 4

	"Include untracked files in the diff"
	GIT_DIFF_INCLUDE_UNTRACKED 8

	"Even with GIT_DIFF_INCLUDE_UNTRACKED, an entire untracked
	 *  directory will be marked with only a single entry in the diff
	 *  (a la what core Git does in `git status`); this flag adds *all*
	 *  files under untracked directories as UNTRACKED entries, too.
	"
	GIT_DIFF_RECURSE_UNTRACKED_DIRS 16

	"Include unmodified files in the diff"
	GIT_DIFF_INCLUDE_UNMODIFIED 32

	"Normally, a type change between files will be converted into a
	 *  DELETED record for the old and an ADDED record for the new; this
	 *  options enabled the generation of TYPECHANGE delta records.
	"
	GIT_DIFF_INCLUDE_TYPECHANGE 64

	"Even with GIT_DIFF_INCLUDE_TYPECHANGE, blob->tree changes still
	 *  generally show as a DELETED blob.  This flag tries to correctly
	 *  label blob->tree transitions as TYPECHANGE records with new_file's
	 *  mode set to tree.  Note: the tree SHA will not be available.
	"
	GIT_DIFF_INCLUDE_TYPECHANGE_TREES 128

	"Ignore file mode changes"
	GIT_DIFF_IGNORE_FILEMODE 256

	"Treat all submodules as unmodified"
	GIT_DIFF_IGNORE_SUBMODULES 512

	"Use case insensitive filename comparisons"
	GIT_DIFF_IGNORE_CASE 1024

	"If the pathspec is set in the diff options, this flags means to
	 *  apply it as an exact match instead of as an fnmatch pattern.
	"
	GIT_DIFF_DISABLE_PATHSPEC_MATCH 4096

	"Disable updating of the `binary` flag in delta records.  This is
	 *  useful when iterating over a diff if you don't need hunk and data
	 *  callbacks and want to avoid having to load file completely.
	"
	GIT_DIFF_SKIP_BINARY_CHECK 8192

	"When diff finds an untracked directory, to match the behavior of
	 *  core Git, it scans the contents for IGNORED and UNTRACKED files.
	 *  If *all* contents are IGNORED, then the directory is IGNORED; if
	 *  any contents are not IGNORED, then the directory is UNTRACKED.
	 *  This is extra work that may not matter in many cases.  This flag
	 *  turns off that scan and immediately labels an untracked directory
	 *  as UNTRACKED (changing the behavior to not match core Git).
	"
	GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS 16384

	"When diff finds a file in the working directory with stat
	 * information different from the index, but the OID ends up being the
	 * same, write the correct stat information into the index.  Note:
	 * without this flag, diff will always leave the index untouched.
	"
	GIT_DIFF_UPDATE_INDEX 32768

	"Include unreadable files in the diff"
	GIT_DIFF_INCLUDE_UNREADABLE 65536
	
	"Include unreadable files in the diff"
	GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED 131072

	"
	 * Options controlling how output will be generated
	"

	"Treat all files as text, disabling binary attributes & detection"
	GIT_DIFF_FORCE_TEXT 1048576
	"Treat all files as binary, disabling text diffs"
	GIT_DIFF_FORCE_BINARY 2097152

	"Ignore all whitespace"
	GIT_DIFF_IGNORE_WHITESPACE 4194304
	"Ignore changes in amount of whitespace"
	GIT_DIFF_IGNORE_WHITESPACE_CHANGE 8388608
	"Ignore whitespace at end of line"
	GIT_DIFF_IGNORE_WHITESPACE_EOL 16777216

	"When generating patch text, include the content of untracked
	 *  files.  This automatically turns on GIT_DIFF_INCLUDE_UNTRACKED but
	 *  it does not turn on GIT_DIFF_RECURSE_UNTRACKED_DIRS.  Add that
	 *  flag if you want the content of every single UNTRACKED file.
	"
	GIT_DIFF_SHOW_UNTRACKED_CONTENT 33554432

	"When generating output, include the names of unmodified files if
	 *  they are included in the git_diff.  Normally these are skipped in
	 *  the formats that list files (e.g. name-only, name-status, raw).
	 *  Even with this, these will not be included in patch format.
	"
	GIT_DIFF_SHOW_UNMODIFIED 67108864

	"Use the 'patience diff' algorithm"
	GIT_DIFF_PATIENCE 268435456
	"Take extra time to find minimal diff"
	GIT_DIFF_MINIMAL 536870912

	"Include the necessary deflate / delta information so that `git-apply`
	 *  can apply given diff information to binary files.
	"
	GIT_DIFF_SHOW_BINARY 1073741824
	)
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_disable_pathspec_match [
	^ self basicNew
		value: 4096;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_enable_fast_untracked_dirs [
	^ self basicNew
		value: 16384;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_force_binary [
	^ self basicNew
		value: 2097152;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_force_text [
	^ self basicNew
		value: 1048576;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_case [
	^ self basicNew
		value: 1024;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_filemode [
	^ self basicNew
		value: 256;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_submodules [
	^ self basicNew
		value: 512;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_whitespace [
	^ self basicNew
		value: 4194304;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_whitespace_change [
	^ self basicNew
		value: 8388608;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_ignore_whitespace_eol [
	^ self basicNew
		value: 16777216;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_ignored [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_typechange [
	^ self basicNew
		value: 64;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_typechange_trees [
	^ self basicNew
		value: 128;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_unmodified [
	^ self basicNew
		value: 32;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_unreadable [
	^ self basicNew
		value: 65536;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_unreadable_as_untracked [
	^ self basicNew
		value: 131072;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_include_untracked [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_minimal [
	^ self basicNew
		value: 536870912;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_normal [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_patience [
	^ self basicNew
		value: 268435456;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_recurse_ignored_dirs [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_recurse_untracked_dirs [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_reverse [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_show_binary [
	^ self basicNew
		value: 1073741824;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_show_unmodified [
	^ self basicNew
		value: 67108864;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_show_untracked_content [
	^ self basicNew
		value: 33554432;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_skip_binary_check [
	^ self basicNew
		value: 8192;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffOptionTypeEnum class >> git_diff_update_index [
	^ self basicNew
		value: 32768;
		yourself
]
