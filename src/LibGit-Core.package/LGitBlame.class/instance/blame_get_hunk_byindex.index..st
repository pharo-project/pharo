libgit-calls
blame_get_hunk_byindex: blame index: index
	"todo: using LGitBlameHunk* as return value causes random crashes"
	
	^ self
		call: #(void * git_blame_get_hunk_byindex #(self , FFIUInt32 index))
		options: #()