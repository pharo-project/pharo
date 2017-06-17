enum declaration
enumDecl
	^#(
	GIT_PATHSPEC_DEFAULT 0
	"forces match to ignore case; otherwise match will use native case sensitivity of platform filesystem"
	GIT_PATHSPEC_IGNORE_CASE 1
	"forces case sensitive match; otherwise match will use native case sensitivity of platform filesystem"
	GIT_PATHSPEC_USE_CASE 2
	"disables glob patterns and just uses simple string comparison for matching"
	GIT_PATHSPEC_NO_GLOB 4
	"means the match functions return error code GIT_ENOTFOUND if no matches are found; otherwise no matches is still success (return 0) but git_pathspec_match_list_entrycount will indicate 0 matches."
	GIT_PATHSPEC_NO_MATCH_ERROR 8
	"means that the git_pathspec_match_list should track which patterns matched which files so that at the end of the match we can identify patterns that did not match any files."
	GIT_PATHSPEC_FIND_FAILURES 16
	"means that the git_pathspec_match_list does not need to keep the actual matching filenames. Use this to just test if there were any matches at all or in combination with GIT_PATHSPEC_FIND_FAILURES to validate a pathspec."
	GIT_PATHSPEC_FAILURES_ONLY 32
)