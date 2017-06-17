libgit-calls
cred_username_new: out username: usernameString
	^ self 
		call: #(int git_cred_username_new (LGitCredentials * out, String usernameString)) 
		options: #()
			