libgit-calls
commit_raw_header: aCommit
	^ self call: #(const char * git_commit_raw_header(self))