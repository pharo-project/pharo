libgit-calls
remote_create_with_fetchspec: out repo: repo name: name url: urlString fetch: fetch
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_remote_create_with_fetchspec #(LGitRemote * self , LGitRepository repo , String name , String urlString , String fetch))
		options: #()