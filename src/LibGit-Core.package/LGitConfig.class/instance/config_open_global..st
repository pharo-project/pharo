libgit-calls
config_open_global: out
	^ self call: #(int git_config_open_global("LGitConfig **"void **out, self))