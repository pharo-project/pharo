enum declaration
enumDecl
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