libgit-calls
diff_get_delta: diff idx: idx
	<todo: 'use LGitDiffDelta* as return type'>
	
	^ self
		call: #(void * git_diff_get_delta #(self , FFIUInt32 idx))
		options: #()