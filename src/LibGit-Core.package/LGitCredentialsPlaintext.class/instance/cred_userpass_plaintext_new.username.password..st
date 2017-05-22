libgit-calls
cred_userpass_plaintext_new: out username: usernameString password: password 
	^self call: #(
		#LGitReturnCodeEnum git_cred_userpass_plaintext_new(
			LGitCredentials * out, 
			String username, 
			String password)
	) options: #(  )