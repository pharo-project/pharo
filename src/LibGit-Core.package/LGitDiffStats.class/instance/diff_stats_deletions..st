libgit-calls
diff_stats_deletions: stats
	
	^ self
		call: #(FFIUInt32 git_diff_stats_deletions #(self))
		options: #()