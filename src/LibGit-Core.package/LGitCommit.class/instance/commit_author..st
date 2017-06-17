libgit-calls
commit_author: commit
	
	^ self
		call: #(LGitSignature * git_commit_author #(self))
		options: #()