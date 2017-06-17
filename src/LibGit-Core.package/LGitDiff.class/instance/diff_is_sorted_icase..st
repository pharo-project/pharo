libgit-calls
diff_is_sorted_icase: diff
	
	^ self call: #(Boolean git_diff_is_sorted_icase #(self)) options: #()