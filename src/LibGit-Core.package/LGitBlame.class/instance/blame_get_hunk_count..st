libgit-calls
blame_get_hunk_count: blame
	
	^ self
		call: #(FFIUInt32 git_blame_get_hunk_count #(self))
		options: #()