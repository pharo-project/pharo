libgit-calls
commit_parent_id: commit n: n
	<todo: 'using LGitId * as return type causes a crash'>
	
	^ self
		call: #(void * git_commit_parent_id #(self , FFIUInt32 n))
		options: #()