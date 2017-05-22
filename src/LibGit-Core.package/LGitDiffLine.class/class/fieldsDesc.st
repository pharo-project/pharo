fields description
fieldsDesc
	^#(
	LGitDiffLineTypeEnum	  origin       "A git_diff_line_t value"
	#FFIInt32    old_lineno   "Line number in old file or -1 for added line"
	#FFIInt32    new_lineno   "Line number in new file or -1 for deleted line"
	#FFIInt32    num_lines    "Number of newline characters in content"
	#FFIUInt32 content_len  "Number of bytes of data"
	#FFIInt64 content_offset "Offset in the original file to the content"
	#FFIExternalString content "Pointer to diff text, not NUL-byte terminated"
	) 