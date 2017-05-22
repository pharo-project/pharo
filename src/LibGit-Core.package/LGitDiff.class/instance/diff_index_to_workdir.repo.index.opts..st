libgit-calls
diff_index_to_workdir: diff repo: repo index: index opts: opts
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_index_to_workdir #(LGitDiff * self , LGitRepository repo , LGitIndex index , LGitDiffOptions * opts))
		options: #()