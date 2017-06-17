libgit-calls
diff_tree_to_tree: diff repo: repo old_tree: old_tree new_tree: new_tree opts: opts
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_diff_tree_to_tree #(LGitDiff * diff , LGitRepository repo , LGitTree old_tree , LGitTree new_tree , LGitDiffOptions * opts))
		options: #()