libgit-calls
diff_stats_insertions: stats
	
	^ self
		call: #(FFIUInt32 git_diff_stats_insertions #(self))
		options: #()