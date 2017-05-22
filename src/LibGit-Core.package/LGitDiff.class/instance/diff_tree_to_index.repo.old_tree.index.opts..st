libgit-calls
diff_tree_to_index: diff repo: repo old_tree: old_tree index: index opts: opts
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_index #(LGitDiff * self , LGitRepository repo , LGitTree old_tree , LGitIndex index , LGitDiffOptions * opts))
		options: #()