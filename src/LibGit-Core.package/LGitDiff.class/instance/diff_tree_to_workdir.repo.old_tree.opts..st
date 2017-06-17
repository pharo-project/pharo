libgit-calls
diff_tree_to_workdir: diff repo: repo old_tree: old_tree opts: opts
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_workdir #(LGitDiff * self , LGitRepository repo , LGitTree old_tree , LGitDiffOptions * opts))
		options: #()