fields description
fieldsDesc
	<hack: 'orig_start_line_number is a uint16_t according to spec. The offset of orig_signature however is 56, not 54 (while the first fields are *not* aligned). To fix that, I changed the type of orig_start_line_number to uint32_t'>
	^ #(
		#FFIUInt16 lines_in_hunk "is the number of lines in this hunk"
		LGitId final_commit_id "is the OID of the commit where this line was last changed."
		#FFIUInt16 final_start_line_number "is the 1-based line number where this hunk begins, in the final version of the file"
		LGitSignature * final_signature
		
		LGitId orig_commit_id "is the OID of the commit where this hunk was found. This will usually be the same as final_commit_id, except when GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES has been specified."
		String orig_path "is the path to the file where this hunk originated, as of the commit specified by orig_commit_id."
		#FFIUInt32 orig_start_line_number "is the 1-based line number where this hunk begins in the file named by orig_path in the commit specified by orig_commit_id."
		LGitSignature * orig_signature
		
		Boolean boundary "is 1 iff the hunk has been tracked to a boundary commit (the root, or the commit specified in git_blame_options.oldest_commit)"
	)