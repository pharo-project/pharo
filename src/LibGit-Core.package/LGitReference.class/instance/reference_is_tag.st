libgit-calls
reference_is_tag	
	^ self call: #(int git_reference_is_tag #(self)) options: #()