enum declaration
enumDecl
	^#(
	GIT_DELTA_UNMODIFIED 0 "no changes"
	GIT_DELTA_ADDED 1 "entry does not exist in old version"
	GIT_DELTA_DELETED 2 "entry does not exist in new version"
	GIT_DELTA_MODIFIED 3 "entry content changed between old and new"
	GIT_DELTA_RENAMED 4 "entry was renamed between old and new"
	GIT_DELTA_COPIED 5 "entry was copied from another old entry"
	GIT_DELTA_IGNORED 6 "entry is ignored item in workdir"
	GIT_DELTA_UNTRACKED 7 "entry is untracked item in workdir"
	GIT_DELTA_TYPECHANGE 8 "type of entry changed between old and new"
	GIT_DELTA_UNREADABLE 9 "entry is unreadable"
	GIT_DELTA_CONFLICTED 10 "< entry in the index is conflicted"
)