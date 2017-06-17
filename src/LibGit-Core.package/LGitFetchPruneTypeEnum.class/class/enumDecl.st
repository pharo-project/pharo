enum declaration
enumDecl
	^ #(
		"
		 * Use the setting from the configuration
		 "
		GIT_FETCH_PRUNE_UNSPECIFIED 0
		"
		 * Force pruning on
		 "
		GIT_FETCH_PRUNE 1
		"
		 * Force pruning off
		 "
		GIT_FETCH_NO_PRUNE 2
	)