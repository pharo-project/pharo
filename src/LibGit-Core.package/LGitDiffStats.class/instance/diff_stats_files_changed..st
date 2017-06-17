libgit-calls
diff_stats_files_changed: stats
	
	^ self
		call: #(FFIUInt32 git_diff_stats_files_changed #(self))
		options: #()