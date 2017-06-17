libgit-calls
commit_parentcount: commit
	
	^ self call: #(uint git_commit_parentcount #(self)) options: #()