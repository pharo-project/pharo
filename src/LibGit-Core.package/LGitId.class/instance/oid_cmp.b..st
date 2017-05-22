libgit-calls
oid_cmp: a b: b 
	^self call: #(FFIInt32 git_oid_cmp(LGitId * self, LGitId * b)) options: #(  )