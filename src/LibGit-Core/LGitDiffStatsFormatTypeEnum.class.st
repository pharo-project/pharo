Class {
	#name : #LGitDiffStatsFormatTypeEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum declaration' }
LGitDiffStatsFormatTypeEnum class >> enumDecl [
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
]

{ #category : #'accessing-values' }
LGitDiffStatsFormatTypeEnum class >> git_diff_stats_full [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffStatsFormatTypeEnum class >> git_diff_stats_include_summary [
	^ self basicNew
		value: 8;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffStatsFormatTypeEnum class >> git_diff_stats_none [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffStatsFormatTypeEnum class >> git_diff_stats_number [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitDiffStatsFormatTypeEnum class >> git_diff_stats_short [
	^ self basicNew
		value: 2;
		yourself
]
