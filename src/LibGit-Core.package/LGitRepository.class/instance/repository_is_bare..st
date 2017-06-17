libgit-calls
repository_is_bare: repo
	
	^ self call: #(Boolean git_repository_is_bare #(self)) options: #()