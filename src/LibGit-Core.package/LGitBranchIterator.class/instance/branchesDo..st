operations
branchesDo: aBlock
	| returnCode branchType reference finishedCode isLocal |
	
	returnCode := LGitReturnCodeEnum git_ok.
	finishedCode := LGitReturnCodeEnum git_iterover.
	branchType := LGitWriteBackValueUInt new.
	
	[ 
		reference := LGitReference of: self repository.
		returnCode := self branch_next: reference out_type: branchType.
		isLocal := branchType value = LGitBranchTypeEnum git_branch_local value.
		returnCode = finishedCode 
	] 
	whileFalse: [ aBlock cull: reference cull: isLocal ]