enum declaration
enumDecl
	^#(
	"No stats"
	GIT_DIFF_STATS_NONE 0

	"Full statistics equivalent of `--stat`"
	GIT_DIFF_STATS_FULL 1

	"Short statistics equivalent of `--shortstat`"
	GIT_DIFF_STATS_SHORT 2

	"Number statistics equivalent of `--numstat`"
	GIT_DIFF_STATS_NUMBER 4

	"Extended header information such as creations renames and mode changes equivalent of `--summary`"
	GIT_DIFF_STATS_INCLUDE_SUMMARY 8
	)