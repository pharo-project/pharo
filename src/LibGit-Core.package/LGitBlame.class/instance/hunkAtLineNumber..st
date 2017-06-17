operations
hunkAtLineNumber: anInteger
	"first line has index 1"
	^ LGitBlameHunk fromExternalAddress: (
		self
			blame_get_hunk_byline: self
			lineno: anInteger)