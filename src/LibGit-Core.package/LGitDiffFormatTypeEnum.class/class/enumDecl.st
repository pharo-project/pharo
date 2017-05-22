enum declaration
enumDecl
	^#(
	GIT_DIFF_FORMAT_PATCH        1 "full git diff"
	GIT_DIFF_FORMAT_PATCH_HEADER 2 "just the file headers of patch"
	GIT_DIFF_FORMAT_RAW          3 "like git diff --raw"
	GIT_DIFF_FORMAT_NAME_ONLY    4 "like git diff --name-only"
	GIT_DIFF_FORMAT_NAME_STATUS  5 "like git diff --name-status"
	)