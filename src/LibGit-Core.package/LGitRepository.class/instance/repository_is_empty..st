libgit-calls
repository_is_empty: repo
	
	^ self call: #(Boolean git_repository_is_empty #(self)) options: #()