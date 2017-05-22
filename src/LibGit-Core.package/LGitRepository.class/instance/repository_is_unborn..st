libgit-calls
repository_is_unborn: repo
	^ self call: #( Boolean git_repository_head_unborn (self) ) options: #()