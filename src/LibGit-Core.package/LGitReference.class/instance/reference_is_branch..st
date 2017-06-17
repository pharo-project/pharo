libgit-calls
reference_is_branch: ref
	
	^ self call: #(int git_reference_is_branch #(self)) options: #()