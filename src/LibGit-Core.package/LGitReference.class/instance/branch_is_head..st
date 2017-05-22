libgit-calls
branch_is_head: branch
	
	^ self call: #(FFIInt32 git_branch_is_head #(self)) options: #()