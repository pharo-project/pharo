testing
isProperRepository
	| return |
	return := self
		repository_open_ext: self
		path: self repositoryReference pathString
		flags: LGitOpenFlagEnum git_repository_open_no_search
		ceiling_dirs: nil.
		
	^ return isOk