libgit-calls
config_free: handle
	^ self
		call: #(void git_config_free #(void *handle))
		options: #()