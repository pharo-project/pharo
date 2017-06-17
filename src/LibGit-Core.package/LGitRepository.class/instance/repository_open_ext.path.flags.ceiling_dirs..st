libgit-calls
repository_open_ext: out path: path flags: flags ceiling_dirs: ceiling_dirs
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_repository_open_ext #(LGitRepository * out , String path , LGitOpenFlagEnum flags , String ceiling_dirs))
		options: #(optCoerceNilToNull optStringOrNull)