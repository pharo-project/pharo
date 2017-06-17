libgit-calls
remote_ls: out size: size
	"
	Since I will take a lost of addresses, I'm interested on 2nd level of 'out', that's why 
	I transform `const git_remote_head ***out` into `void **out`
	This is the original function: 
		int git_remote_ls(const git_remote_head ***out, size_t *size, git_remote *remote)"
	^ self call: #( LGitReturnCodeEnum git_remote_ls ( void **out, size_t *size, self ))