libgit-calls
diff_stats_to_buf: out stats: stats format: format width: width
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_stats_to_buf #(LGitBuffer out , self , LGitDiffStatsFormatTypeEnum format , FFIUInt32 width))
		options: #()