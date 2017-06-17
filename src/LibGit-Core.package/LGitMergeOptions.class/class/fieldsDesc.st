accessing
fieldsDesc
	"https://libgit2.github.com/libgit2/#HEAD/type/git_merge_options"
	^#(
		LGitOptionsVersionsEnum version;
		#FFIUInt32 flags; "Should be: git_merge_flag_t"
		#FFIUInt32 rename_threshold;
		#FFIUInt32 target_limit;
		#FFIVoid * metric; "Should be: git_diff_similarity_metric *"
		#FFIUInt32 file_favor; "Should be: git_merge_favor_flag_t"
		#FFIUInt32 file_flags; "Should be: git_merge_file_flag_t"
	)