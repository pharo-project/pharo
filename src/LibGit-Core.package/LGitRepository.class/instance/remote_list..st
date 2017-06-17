libgit-calls
remote_list: out
	^ self 
		call: #( int git_remote_list ( LGitStringArray *out, self ) ) 
		options: #()