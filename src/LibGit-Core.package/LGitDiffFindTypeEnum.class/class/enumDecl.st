enum declaration
enumDecl
	<todo: 'enum defines 0 twice (one commented out). Use second enum?'>
	^#(
	" Obey `diff.renames`. Overridden by any other GIT_DIFF_FIND_... flag. "
	GIT_DIFF_FIND_BY_CONFIG 0

	" Look for renames? (`--find-renames`) "
	GIT_DIFF_FIND_RENAMES 1

	" Consider old side of MODIFIED for renames? (`--break-rewrites=N`) "
	GIT_DIFF_FIND_RENAMES_FROM_REWRITES 2

	" Look for copies? (a la `--find-copies`). "
	GIT_DIFF_FIND_COPIES 4

	" Consider UNMODIFIED as copy sources? (`--find-copies-harder`).
	 *
	 * For this to work correctly use GIT_DIFF_INCLUDE_UNMODIFIED when
	 * the initial `git_diff` is being generated.
	 "
	GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED 8

	" Mark significant rewrites for split (`--break-rewrites=/M`) "
	GIT_DIFF_FIND_REWRITES 16
	" Actually split large rewrites into delete/add pairs "
	GIT_DIFF_BREAK_REWRITES 32
	" Mark rewrites for split and break into delete/add pairs "
	GIT_DIFF_FIND_AND_BREAK_REWRITES 48

	" Find renames/copies for UNTRACKED items in working directory.
	 *
	 * For this to work correctly use GIT_DIFF_INCLUDE_UNTRACKED when the
	 * initial `git_diff` is being generated (and obviously the diff must
	 * be against the working directory for this to make sense).
	 "
	GIT_DIFF_FIND_FOR_UNTRACKED 64

	" Turn on all finding features. "
	GIT_DIFF_FIND_ALL 255

	" Measure similarity ignoring leading whitespace (default) "
	"GIT_DIFF_FIND_IGNORE_LEADING_WHITESPACE 0"
	" Measure similarity ignoring all whitespace "
	GIT_DIFF_FIND_IGNORE_WHITESPACE 4096
	" Measure similarity including all data "
	GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE 8192
	" Measure similarity only by comparing SHAs (fast and cheap) "
	GIT_DIFF_FIND_EXACT_MATCH_ONLY 16384

	" Do not break rewrites unless they contribute to a rename.
	 *
	 * Normally GIT_DIFF_FIND_AND_BREAK_REWRITES will measure the self-
	 * similarity of modified files and split the ones that have changed a
	 * lot into a DELETE / ADD pair.  Then the sides of that pair will be
	 * considered candidates for rename and copy detection.
	 *
	 * If you add this flag in and the split pair is *not* used for an
	 * actual rename or copy then the modified record will be restored to
	 * a regular MODIFIED record instead of being split.
	 "
	GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY 32768

	" Remove any UNMODIFIED deltas after find_similar is done.
	 *
	 * Using GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED to emulate the
	 * --find-copies-harder behavior requires building a diff with the
	 * GIT_DIFF_INCLUDE_UNMODIFIED flag.  If you do not want UNMODIFIED
	 * records in the final result pass this flag to have them removed.
	 "
	GIT_DIFF_FIND_REMOVE_UNMODIFIED 65536
	)