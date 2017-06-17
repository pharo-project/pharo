libgit-calls
blame_get_hunk_byline: blame lineno: lineno
	"todo: using LGitBlameHunk* as return value causes random crashes"
	
	^ self
		call: #(void * git_blame_get_hunk_byline #(self , FFIUInt32 lineno))
		options: #()