libgit-calls
diff_num_deltas: diff
	
	^ self call: #(FFIUInt32 git_diff_num_deltas #(self)) options: #()