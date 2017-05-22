libgit-calls
tag_free: handle
	^ self call: #(void git_tag_free #(void *handle)) options: #()