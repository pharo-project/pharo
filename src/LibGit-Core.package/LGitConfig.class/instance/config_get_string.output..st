libgit-calls
config_get_string: name output: output
	^ self call:
			#(LGitReturnCodeEnum git_config_get_string_buf(
				LGitBuf* output, 
				self, 
				String name))
		options: #()