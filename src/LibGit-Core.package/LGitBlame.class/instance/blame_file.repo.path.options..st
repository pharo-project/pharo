libgit-calls
blame_file: out repo: repo path: path options: options
	
	^ self
		callUnchecked:
			#(FFIInt32 git_blame_file #(LGitBlame * self , LGitRepository repo , FFIExternalString path , LGitBlameOptions * options))
		options: #()