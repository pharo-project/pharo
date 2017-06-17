enum declaration
enumDecl
	^#(
		GIT_STATUS_CURRENT  0
		GIT_STATUS_INDEX_NEW         "1 bitShift: 0" 1
		GIT_STATUS_INDEX_MODIFIED    "1 bitShift: 1" 2
		GIT_STATUS_INDEX_DELETED     "1 bitShift: 2" 4
		GIT_STATUS_INDEX_RENAMED     "1 bitShift: 3" 8
		GIT_STATUS_INDEX_TYPECHANGE  "1 bitShift: 4" 16
		GIT_STATUS_WT_NEW            "1 bitShift: 7" 128
		GIT_STATUS_WT_MODIFIED       "1 bitShift: 8" 256
		GIT_STATUS_WT_DELETED        "1 bitShift: 9" 512
		GIT_STATUS_WT_TYPECHANGE     "1 bitShift: 10" 1024
		GIT_STATUS_WT_RENAMED        "1 bitShift: 11" 2048
		GIT_STATUS_WT_UNREADABLE     "1 bitShift: 12" 4096
		GIT_STATUS_IGNORED           "1 bitShift: 14" 16384
		GIT_STATUS_CONFLICTED        "1 bitShift: 15" 32768
)