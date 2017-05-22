libgit-calls
branch_lookup: out branch_name: branch_name branch_type: branch_type
	
	^ self
		call:
			#(LGitReturnCodeEnum git_branch_lookup #(LGitReference * out , self , String branch_name , LGitBranchTypeEnum branch_type))
		options: #()