libgit-calls
annotated_commit_free: handle
	^ self
		call: #(void git_annotated_commit_free #(void *handle))
		options: #()