fields description
fieldsDesc
	^ #(
		LGitOptionsVersionsEnum version
		#FFIUInt32 flags "is a combination of the git_blame_flag_t values above."
		#FFIUInt16 min_match_characters "is the lower bound on the number of alphanumeric characters that must be detected as moving/copying within a file for it to associate those lines with the parent commit. The default value is 20. This value only takes effect if any of the GIT_BLAME_TRACK_COPIES_* flags are specified."
		LGitId newest_commit "is the id of the newest commit to consider. The default is HEAD."
		LGitId oldest_commit "is the id of the oldest commit to consider. The default is the first commit encountered with a NULL parent."
		#FFIUInt32 min_line "is the first line in the file to blame. The default is 1 (line numbers start with 1)."
		#FFIUInt32 max_line "is the last line in the file to blame. The default is the last line of the file."
	)