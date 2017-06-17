libgit-calls
index_owner
	^ self call: #(LGitRepository * git_index_owner (LGitIndex self)) options: #()